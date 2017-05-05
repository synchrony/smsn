package net.fortytwo.smsn.typeatron.ripple;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.brain.query.ViewStyle;
import net.fortytwo.smsn.brain.io.json.JsonFormat;
import net.fortytwo.smsn.brain.io.json.JsonParser;
import net.fortytwo.smsn.brain.io.json.JsonPrinter;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.config.Service;
import org.apache.commons.io.IOUtils;
import org.apache.http.ConnectionReuseStrategy;
import org.apache.http.HttpEntity;
import org.apache.http.HttpException;
import org.apache.http.HttpHost;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.impl.DefaultBHttpClientConnection;
import org.apache.http.impl.DefaultConnectionReuseStrategy;
import org.apache.http.message.BasicHttpEntityEnclosingRequest;
import org.apache.http.message.BasicHttpRequest;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HttpCoreContext;
import org.apache.http.protocol.HttpProcessor;
import org.apache.http.protocol.HttpProcessorBuilder;
import org.apache.http.protocol.HttpRequestExecutor;
import org.apache.http.protocol.RequestConnControl;
import org.apache.http.protocol.RequestContent;
import org.apache.http.protocol.RequestExpectContinue;
import org.apache.http.protocol.RequestTargetHost;
import org.apache.http.protocol.RequestUserAgent;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.Socket;
import java.util.LinkedList;
import java.util.List;

public class BrainClient {

    private static final String PROP_SERVER_NAME = "net.fortytwo.smsn.brain.client.serverName";
    private static final String PROP_SERVER_PORT = "net.fortytwo.smsn.brain.client.serverPort";
    private static final String PROP_GRAPH = "net.fortytwo.smsn.brain.client.graph";

    private static final int DEFAULT_VALUE_CUTOFF = 100;

    private final JsonParser jsonParser = new JsonParser();
    private final JsonPrinter jsonPrinter = new JsonPrinter();

    private final HttpProcessor httpProcessor;
    private final HttpRequestExecutor httpExecutor;
    private final HttpCoreContext httpContext;
    private final HttpHost httpHost;

    private final String baseUrl;

    public BrainClient() throws BrainClientException {
        // note: constructor will fail if any of these properties are not defined
        String serverName;
        int serverPort;
        String graph;
        Service config = SemanticSynchrony.getConfiguration().getServices().getServer();
        serverName = config.getHost();
        serverPort = config.getPort();
        graph = config.getGraph();

        baseUrl = "/graphs/" + graph + "/smsn/";

        httpProcessor = HttpProcessorBuilder.create()
                .add(new RequestContent())
                .add(new RequestTargetHost())
                .add(new RequestConnControl())
                // TODO
                //.add(new RequestUserAgent(agentName))
                .add(new RequestUserAgent())
                .add(new RequestExpectContinue()).build();
        httpExecutor = new HttpRequestExecutor();
        httpContext = HttpCoreContext.create();
        httpHost = new HttpHost(serverName, serverPort);
        httpContext.setTargetHost(httpHost);
    }

    /**
     * Generates a view of the graph.
     *
     * @param root   the root atom of the view
     * @param height the height of the view.
     *               A view of height 0 contains only the root,
     *               while a view of height 1 also contains all children of the root,
     *               a view of height 2 all grandchildren, etc.
     * @param filter a collection of criteria for atoms and links.
     *               Atoms and links which do not meet the criteria are not to appear in the view.
     * @param style  the adjacency style of the view
     * @return a partial view of the graph as a tree of <code>Note</code> objects
     */
    public Note view(final Note root,
                     final int height,
                     final Filter filter,
                     final ViewStyle style,
                     final boolean includeTypes) throws BrainClientException {

        if (null == root || null == root.getId() || height < 0 || null == filter || null == style) {
            throw new IllegalArgumentException();
        }

        JSONObject requestJson = new JSONObject();
        try {
            requestJson.put(Params.ROOT, root.getId());
            requestJson.put(Params.HEIGHT, height);
            requestJson.put(Params.STYLE, style.getName());
            requestJson.put(Params.INCLUDE_TYPES, includeTypes);
            requestJson.put(Params.FILTER, toJson(filter));
        } catch (JSONException e) {
            throw new BrainClientException(e);
        }

        List<NameValuePair> params = new LinkedList<>();
        params.add(new BasicNameValuePair(Params.REQUEST, requestJson.toString()));
        String paramStr = URLEncodedUtils.format(params, SemanticSynchrony.UTF8);
        System.out.println("parameter string: " + paramStr);
        String path = baseUrl + "view?" + paramStr;

        final Note[] results = new Note[1];

        HttpResponseHandler handler = response -> {
            int code = response.getStatusLine().getStatusCode();

            if (200 == code) {
                try {
                    JSONObject json = new JSONObject(
                            IOUtils.toString(response.getEntity().getContent(), SemanticSynchrony.UTF8));
                    JSONObject view = json.getJSONObject(Params.VIEW);
                    // note: redundant serialization/deserialization
                    results[0] = jsonParser.parse(view.toString());
                } catch (JSONException e) {
                    throw new IOException(e);
                }
            } else {
                throw new IOException("HTTP response of " + code + " for view request: "
                        + response.getStatusLine().getReasonPhrase());
            }
        };

        try {
            get(handler, path);
        } catch (IOException | HttpException e) {
            throw new BrainClientException(e);
        }

        return results[0];
    }

    /**
     * Updates the graph
     *
     * @param root   the root of the subgraph to be updated
     * @param height the maximum height of the tree which will be applied to the graph as an update.
     *               If height is 0, only the root node will be affected,
     *               while a height of 1 will also affect children (which have a depth of 1 from the root), etc.
     * @param filter a collection of criteria for atoms and links.
     *               Atoms and links which do not meet the criteria are not to be affected by the update.
     * @param style  the adjacency style of the view
     */
    public void update(final Note root,
                       final int height,
                       final Filter filter,
                       final ViewStyle style) throws BrainClientException {

        if (null == root || null == root.getId() || height < 0 || null == filter || null == style) {
            throw new IllegalArgumentException();
        }

        JSONObject requestJson = new JSONObject();
        try {
            requestJson.put(Params.ROOT, root.getId());
            requestJson.put(Params.HEIGHT, height);
            requestJson.put(Params.STYLE, style.getName());
            requestJson.put(Params.VIEW, toJson(root));
            requestJson.put(Params.VIEW_FORMAT, Params.Format.json);
            requestJson.put(Params.FILTER, toJson(filter));
        } catch (IOException | JSONException e) {
            throw new BrainClientException(e);
        }

        List<NameValuePair> params = new LinkedList<>();
        params.add(new BasicNameValuePair(Params.REQUEST, requestJson.toString()));
        UrlEncodedFormEntity ent;
        try {
            ent = new UrlEncodedFormEntity(params);
        } catch (UnsupportedEncodingException e) {
            throw new BrainClientException(e);
        }

        HttpResponseHandler handler = response -> {
            int code = response.getStatusLine().getStatusCode();

            if (200 == code) {
                try {
                    JSONObject json = new JSONObject(
                            IOUtils.toString(response.getEntity().getContent(), SemanticSynchrony.UTF8));
                    JSONObject view = json.getJSONObject(Params.VIEW);
                } catch (JSONException e) {
                    throw new IOException(e);
                }
            } else {
                throw new IOException("HTTP response of " + code + " for view request: "
                        + response.getStatusLine().getReasonPhrase());
            }
        };

        try {
            post(baseUrl + "update", handler, ent);
        } catch (IOException | HttpException e) {
            throw new BrainClientException(e);
        }
    }

    public void setProperty(final Note root,
                            final String name,
                            final String value) throws BrainClientException {
        // TODO: add ability to clear property values
        if (null == root || null == root.getId()
                || null == name || 0 == name.length() || null == value || 0 == value.length()) {
            throw new IllegalArgumentException();
        }

        JSONObject requestJson = new JSONObject();
        try {
            requestJson.put(Params.ID, root.getId());
            requestJson.put(Params.NAME, name);
            requestJson.put(Params.TITLE, value);
        } catch (JSONException e) {
            throw new BrainClientException(e);
        }

        List<NameValuePair> params = new LinkedList<>();
        params.add(new BasicNameValuePair(Params.REQUEST, requestJson.toString()));
        String paramStr = URLEncodedUtils.format(params, SemanticSynchrony.UTF8);
        String path = baseUrl + "set?" + paramStr;

        HttpResponseHandler handler = response -> {
            int code = response.getStatusLine().getStatusCode();

            if (200 != code) {
                throw new IOException("HTTP response of " + code + " for set property request: "
                        + response.getStatusLine().getReasonPhrase());
            }
        };

        try {
            get(handler, path);
        } catch (IOException | HttpException e) {
            throw new BrainClientException(e);
        }
    }

    /**
     * Performs a specified type of search, such as full text or acronym search
     *
     * @param queryType the type of search to perform
     * @param query     the search query
     * @param height    maximum height of the search results view
     * @param filter    a collection of criteria for atoms and links.
     *                  Atoms and links which do not meet the criteria are not to appear in search results.
     * @param style     the adjacency style of the view
     * @return an ordered list of query results
     */
    public List<Note> search(final TreeViews.QueryType queryType,
                             final String query,
                             final int height,
                             final Filter filter,
                             final ViewStyle style) throws BrainClientException {
        if (null == queryType || null == query || 0 == query.length()
                || height < 0 || null == filter || null == style) {
            throw new IllegalArgumentException();
        }

        JSONObject requestJson = new JSONObject();
        try {
            requestJson.put(Params.QUERY_TYPE, queryType.name());
            requestJson.put(Params.QUERY, query);
            requestJson.put(Params.HEIGHT, height);
            requestJson.put(Params.FILTER, toJson(filter));
            requestJson.put(Params.STYLE, style.getName());
            requestJson.put(Params.TITLE_CUTOFF, DEFAULT_VALUE_CUTOFF);
        } catch (JSONException e) {
            throw new BrainClientException(e);
        }
        List<NameValuePair> params = new LinkedList<>();
        params.add(new BasicNameValuePair(Params.REQUEST, requestJson.toString()));
        String paramStr = URLEncodedUtils.format(params, SemanticSynchrony.UTF8);
        String path = baseUrl + "search?" + paramStr;

        final List<Note> results = new LinkedList<>();

        HttpResponseHandler handler = response -> {
            int code = response.getStatusLine().getStatusCode();

            if (200 == code) {
                try {
                    JSONObject json = new JSONObject(
                            IOUtils.toString(response.getEntity().getContent(), SemanticSynchrony.UTF8));
                    JSONObject view = json.getJSONObject(Params.VIEW);
                    JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
                    if (null != children) {
                        int length = children.length();
                        for (int i = 0; i < length; i++) {
                            results.add(jsonParser.parse(children.getJSONObject(i).toString()));
                        }
                    }
                } catch (JSONException e) {
                    throw new IOException(e);
                }
            } else {
                throw new IOException("HTTP response of " + code + " for search request: "
                        + response.getStatusLine().getReasonPhrase());
            }
        };

        try {
            get(handler, path);
        } catch (IOException | HttpException e) {
            throw new BrainClientException(e);
        }

        return results;
    }

    private void get(final HttpResponseHandler responseHandler,
                     final String... paths) throws IOException, HttpException {

        ConnectionReuseStrategy connStrategy = new DefaultConnectionReuseStrategy();

        try (DefaultBHttpClientConnection conn = new DefaultBHttpClientConnection(8 * 1024)) {
            for (String path : paths) {
                if (!conn.isOpen()) {
                    Socket socket = new Socket(httpHost.getHostName(), httpHost.getPort());
                    conn.bind(socket);
                }
                BasicHttpRequest request = new BasicHttpRequest("GET", path);
                System.out.println(">> Request IRI: " + request.getRequestLine().getUri());

                httpExecutor.preProcess(request, httpProcessor, httpContext);
                HttpResponse response = httpExecutor.execute(request, conn, httpContext);
                httpExecutor.postProcess(response, httpProcessor, httpContext);

                responseHandler.handle(response);

                if (!connStrategy.keepAlive(response, httpContext)) {
                    conn.close();
                } else {
                    System.out.println("Connection kept alive...");
                }
            }
        }
    }

    private void post(final String path,
                      final HttpResponseHandler responseHandler,
                      final HttpEntity... requests) throws IOException, HttpException {

        ConnectionReuseStrategy connStrategy = new DefaultConnectionReuseStrategy();

        try (DefaultBHttpClientConnection conn = new DefaultBHttpClientConnection(8 * 1024)) {
            for (HttpEntity requestBody : requests) {
                if (!conn.isOpen()) {
                    Socket socket = new Socket(httpHost.getHostName(), httpHost.getPort());
                    conn.bind(socket);
                }
                BasicHttpEntityEnclosingRequest request = new BasicHttpEntityEnclosingRequest("POST",
                        path);
                request.setEntity(requestBody);
                System.out.println(">> Request IRI: " + request.getRequestLine().getUri());

                httpExecutor.preProcess(request, httpProcessor, httpContext);
                HttpResponse response = httpExecutor.execute(request, conn, httpContext);
                httpExecutor.postProcess(response, httpProcessor, httpContext);

                responseHandler.handle(response);

                if (!connStrategy.keepAlive(response, httpContext)) {
                    conn.close();
                } else {
                    System.out.println("Connection kept alive...");
                }
            }
        }
    }

    private interface HttpResponseHandler {
        void handle(HttpResponse response) throws IOException;
    }

    private JSONObject toJson(final Filter filter) throws JSONException {
        JSONObject json = new JSONObject();
        json.put(Params.MIN_SHARABILITY, filter.getMinSharability());
        json.put(Params.MIN_WEIGHT, filter.getMinWeight());
        json.put(Params.DEFAULT_SHARABILITY, filter.getDefaultSharability());
        json.put(Params.DEFAULT_WEIGHT, filter.getDefaultWeight());
        return json;
    }

    private JSONObject toJson(final Note note) throws IOException {
        return jsonPrinter.toJson(note);
    }

    public class BrainClientException extends Exception {
        public BrainClientException(Throwable cause) {
            super(cause);
        }
    }
}
