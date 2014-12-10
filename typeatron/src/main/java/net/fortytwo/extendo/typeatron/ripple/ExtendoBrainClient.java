package net.fortytwo.extendo.typeatron.ripple;

import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Filter;
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.extendo.brain.NoteQueries;
import net.fortytwo.extendo.brain.Params;
import net.fortytwo.extendo.brain.wiki.NoteParser;
import net.fortytwo.extendo.brain.wiki.NoteWriter;
import net.fortytwo.extendo.util.TypedProperties;
import net.fortytwo.ripple.RippleException;
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
import java.net.Socket;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExtendoBrainClient {
    private static final Logger logger = Logger.getLogger(ExtendoBrainClient.class.getName());

    public static final String PROP_SERVER_NAME = "net.fortytwo.extendo.brain.client.serverName",
            PROP_SERVER_PORT = "net.fortytwo.extendo.brain.client.serverPort",
            PROP_GRAPH = "net.fortytwo.extendo.brain.client.graph";

    private static final int DEFAULT_VALUE_CUTOFF = 100;

    private final NoteWriter noteWriter = new NoteWriter();
    private final NoteParser noteParser = new NoteParser();

    private final HttpProcessor httpProcessor;
    private final HttpRequestExecutor httpExecutor;
    private final HttpCoreContext httpContext;
    private final HttpHost httpHost;

    private final String baseUrl;

    public ExtendoBrainClient() throws RippleException {
        TypedProperties config = Extendo.getConfiguration();
        if (null == config) {
            throw new IllegalStateException();
        }

        // note: constructor will fail if any of these properties are not defined
        String agentName = null;
        String serverName = null;
        int serverPort = 0;
        String graph = null;
        try {
            agentName = "ExtendoClient/" + config.getString(Extendo.VERSION);
            serverName = config.getString(PROP_SERVER_NAME);
            serverPort = config.getInt(PROP_SERVER_PORT);
            graph = config.getString(PROP_GRAPH);
        } catch (TypedProperties.PropertyException e) {
            throw new RippleException(e);
        }

        baseUrl = "/graphs/" + graph + "/extendo/";

        httpProcessor = HttpProcessorBuilder.create()
                .add(new RequestContent())
                .add(new RequestTargetHost())
                .add(new RequestConnControl())
                .add(new RequestUserAgent(agentName))
                .add(new RequestExpectContinue(true)).build();
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
                     final NoteQueries.AdjacencyStyle style,
                     final boolean includeTypes) throws JSONException, IOException, HttpException {

        if (null == root || null == root.getId() || height < 0 || null == filter || null == style) {
            throw new IllegalArgumentException();
        }

        JSONObject requestJson = new JSONObject();
        requestJson.put(Params.ROOT, root.getId());
        requestJson.put(Params.DEPTH, height);
        requestJson.put(Params.STYLE, style.getName());
        requestJson.put(Params.INCLUDE_TYPES, includeTypes);
        requestJson.put(Params.FILTER, toJson(filter));

        List<NameValuePair> params = new LinkedList<NameValuePair>();
        params.add(new BasicNameValuePair(Params.REQUEST, requestJson.toString()));
        String paramStr = URLEncodedUtils.format(params, Extendo.UTF8);
        System.out.println("parameter string: " + paramStr);
        String path = baseUrl + "view?" + paramStr;

        final Note[] results = new Note[1];

        HttpResponseHandler handler = new HttpResponseHandler() {
            @Override
            public void handle(HttpResponse response) throws IOException {
                int code = response.getStatusLine().getStatusCode();

                if (200 == code) {
                    try {
                        JSONObject json = new JSONObject(
                                IOUtils.toString(response.getEntity().getContent(), Extendo.UTF8));
                        JSONObject view = json.getJSONObject(Params.VIEW);
                        results[0] = noteParser.fromJSON(view);
                    } catch (JSONException e) {
                        throw new IOException(e);
                    }
                } else {
                    throw new IOException("HTTP response of " + code + " for view request: "
                            + response.getStatusLine().getReasonPhrase());
                }
            }
        };

        get(handler, path);

        return results[0];
    }

    /**
     * Updates the graph
     *
     * @param root   the root of the subgraph to be updated
     * @param depth  the minimum depth to which the graph will be updated.
     *               If depth is 0, only the root node will be affected,
     *               while a depth of 1 will affect children (which have a depth of 1 from the root), etc.
     * @param filter a collection of criteria for atoms and links.
     *               Atoms and links which do not meet the criteria are not to be affected by the update.
     * @param style  the adjacency style of the view
     */
    public Note update(final Note root,
                       final int depth,
                       final Filter filter,
                       final NoteQueries.AdjacencyStyle style) throws JSONException, IOException, HttpException {

        if (null == root || null == root.getId() || depth < 0 || null == filter || null == style) {
            throw new IllegalArgumentException();
        }

        JSONObject requestJson = new JSONObject();
        requestJson.put(Params.ROOT, root.getId());
        requestJson.put(Params.DEPTH, depth);
        requestJson.put(Params.STYLE, style.getName());
        requestJson.put(Params.VIEW, toJson(root));
        requestJson.put(Params.FILTER, toJson(filter));

        List<NameValuePair> params = new LinkedList<NameValuePair>();
        params.add(new BasicNameValuePair(Params.REQUEST, requestJson.toString()));
        UrlEncodedFormEntity ent = new UrlEncodedFormEntity(params);

        final Note[] results = new Note[1];

        HttpResponseHandler handler = new HttpResponseHandler() {
            @Override
            public void handle(HttpResponse response) throws IOException {
                int code = response.getStatusLine().getStatusCode();

                if (200 == code) {
                    try {
                        JSONObject json = new JSONObject(
                                IOUtils.toString(response.getEntity().getContent(), Extendo.UTF8));
                        JSONObject view = json.getJSONObject(Params.VIEW);
                        results[0] = noteParser.fromJSON(view);
                    } catch (JSONException e) {
                        throw new IOException(e);
                    }
                } else {
                    throw new IOException("HTTP response of " + code + " for view request: "
                            + response.getStatusLine().getReasonPhrase());
                }
            }
        };

        post(baseUrl + "update", handler, ent);

        return results[0];
        /*
                (http-post
            (concat (base-url) "update")
            (list
                (list "request" (json-encode (list
                    :root exo-root-id
                    :depth (number-to-string exo-depth)
                    :style exo-style
                    :view entity
                    :filter (filter-json exo-min-sharability exo-max-sharability exo-default-sharability exo-min-weight exo-max-weight exo-default-weight)))))
            (receive-view exo-edit-mode))))
    (sit-for 0 500)(exo-update-view)(sit-for 0 500)(exo-update-view)) ;; TODO: this is a hack to get around the 405 issue on the server

         */
    }

    public void setProperty(final Note root,
                            final String name,
                            final String value) throws JSONException, IOException, HttpException {
        // TODO: add ability to clear property values
        if (null == root || null == root.getId()
                || null == name || 0 == name.length() || null == value || 0 == value.length()) {
            throw new IllegalArgumentException();
        }

        JSONObject requestJson = new JSONObject();
        requestJson.put(Params.ID, root.getId());
        requestJson.put(Params.NAME, name);
        requestJson.put(Params.VALUE, value);

        List<NameValuePair> params = new LinkedList<NameValuePair>();
        params.add(new BasicNameValuePair(Params.REQUEST, requestJson.toString()));
        String paramStr = URLEncodedUtils.format(params, Extendo.UTF8);
        String path = baseUrl + "set?" + paramStr;

        HttpResponseHandler handler = new HttpResponseHandler() {
            @Override
            public void handle(HttpResponse response) throws IOException {
                int code = response.getStatusLine().getStatusCode();

                if (200 != code) {
                    throw new IOException("HTTP response of " + code + " for set property request: "
                            + response.getStatusLine().getReasonPhrase());
                }
            }
        };

        get(handler, path);
    }

    /**
     * Performs a specified type of search, such as full text or acronym search
     *
     * @param queryType the type of search to perform
     * @param query     the search query
     * @param depth     depth of the search results view
     * @param filter    a collection of criteria for atoms and links.
     *                  Atoms and links which do not meet the criteria are not to appear in search results.
     * @param style     the adjacency style of the view
     * @return an ordered list of query results
     */
    public List<Note> search(final NoteQueries.QueryType queryType,
                       final String query,
                       final int depth,
                       final Filter filter,
                       final NoteQueries.AdjacencyStyle style) throws JSONException, IOException, HttpException {
        if (null == queryType || null == query || 0 == query.length()
                || depth < 0 || null == filter || null == style) {
            throw new IllegalArgumentException();
        }

        JSONObject requestJson = new JSONObject();
        requestJson.put(Params.QUERY_TYPE, queryType.name());
        requestJson.put(Params.QUERY, query);
        requestJson.put(Params.DEPTH, depth);
        requestJson.put(Params.FILTER, toJson(filter));
        requestJson.put(Params.STYLE, style.getName());
        requestJson.put(Params.VALUE_CUTOFF, DEFAULT_VALUE_CUTOFF);

        List<NameValuePair> params = new LinkedList<NameValuePair>();
        params.add(new BasicNameValuePair(Params.REQUEST, requestJson.toString()));
        String paramStr = URLEncodedUtils.format(params, Extendo.UTF8);
        String path = baseUrl + "search?" + paramStr;

        final List<Note> results = new LinkedList<Note>();

        HttpResponseHandler handler = new HttpResponseHandler() {
            @Override
            public void handle(HttpResponse response) throws IOException {
                int code = response.getStatusLine().getStatusCode();

                if (200 == code) {
                    try {
                        JSONObject json = new JSONObject(
                                IOUtils.toString(response.getEntity().getContent(), Extendo.UTF8));
                        JSONObject view = json.getJSONObject(Params.VIEW);
                        JSONArray children = view.optJSONArray(NoteWriter.CHILDREN);
                        if (null != children) {
                            int length = children.length();
                            for (int i = 0; i < length; i++) {
                                results.add(noteParser.fromJSON(children.getJSONObject(i)));
                            }
                        }
                    } catch (JSONException e) {
                        throw new IOException(e);
                    }
                } else {
                    throw new IOException("HTTP response of " + code + " for search request: "
                            + response.getStatusLine().getReasonPhrase());
                }
            }
        };

        get(handler, path);

        return results;
    }

    private void get(final HttpResponseHandler responseHandler,
                     final String... paths) throws IOException, HttpException {

        DefaultBHttpClientConnection conn = new DefaultBHttpClientConnection(8 * 1024);
        ConnectionReuseStrategy connStrategy = DefaultConnectionReuseStrategy.INSTANCE;

        try {
            for (String path : paths) {
                if (!conn.isOpen()) {
                    Socket socket = new Socket(httpHost.getHostName(), httpHost.getPort());
                    conn.bind(socket);
                }
                BasicHttpRequest request = new BasicHttpRequest("GET", path);
                System.out.println(">> Request URI: " + request.getRequestLine().getUri());

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
        } finally {
            conn.close();
        }
    }

    private void post(final String path,
                      final HttpResponseHandler responseHandler,
                      final HttpEntity... requests) throws IOException, HttpException {

        DefaultBHttpClientConnection conn = new DefaultBHttpClientConnection(8 * 1024);
        ConnectionReuseStrategy connStrategy = DefaultConnectionReuseStrategy.INSTANCE;

        try {
            for (HttpEntity requestBody : requests) {
                if (!conn.isOpen()) {
                    Socket socket = new Socket(httpHost.getHostName(), httpHost.getPort());
                    conn.bind(socket);
                }
                BasicHttpEntityEnclosingRequest request = new BasicHttpEntityEnclosingRequest("POST",
                        path);
                request.setEntity(requestBody);
                System.out.println(">> Request URI: " + request.getRequestLine().getUri());

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
        } finally {
            conn.close();
        }
    }

    private interface HttpResponseHandler {
        void handle(HttpResponse response) throws IOException;
    }

    private JSONObject toJson(final Filter filter) throws JSONException {
        JSONObject json = new JSONObject();
        json.put(Params.MIN_SHARABILITY, filter.getMinSharability());
        json.put(Params.MAX_SHARABILITY, filter.getMaxSharability());
        json.put(Params.MIN_WEIGHT, filter.getMinWeight());
        json.put(Params.MAX_WEIGHT, filter.getMaxWeight());
        json.put(Params.DEFAULT_SHARABILITY, filter.getDefaultSharability());
        json.put(Params.DEFAULT_WEIGHT, filter.getDefaultWeight());
        return json;
    }

    private JSONObject toJson(final Note note) throws JSONException {
        return noteWriter.toJSON(note);
    }
}
