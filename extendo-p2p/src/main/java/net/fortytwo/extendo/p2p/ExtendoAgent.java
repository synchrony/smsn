package net.fortytwo.extendo.p2p;

import edu.rpi.twc.sesamestream.QueryEngine;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.p2p.sparql.QueryEngineProxy;
import net.fortytwo.rdfagents.data.DatasetFactory;
import net.fortytwo.rdfagents.model.Dataset;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.NameValuePair;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFWriter;
import org.openrdf.rio.Rio;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExtendoAgent {
    protected static final Logger LOGGER = Logger.getLogger(ExtendoAgent.class.getName());

    public static final String
            PROP_BODY = "body",
            PROP_TAG = "tag";

    protected final URI agentUri;
    protected final DatasetFactory factory = new DatasetFactory();
    protected final ValueFactory vf = factory.getValueFactory();

    private ServiceBroadcastListener listener;

    private Service service;

    private final HttpClient httpclient = new DefaultHttpClient();

    private QueryEngine queryEngine;

    private final Connection facilitatorConnection;

    public ExtendoAgent(final String agentUri,
                        final boolean listenForServices) {
        LOGGER.info("creating Extendo agent with URI " + agentUri);

        this.agentUri = vf.createURI(agentUri);

        facilitatorConnection = new Connection();
        queryEngine = new QueryEngineProxy(facilitatorConnection);

        if (listenForServices) {
            listener = new ServiceBroadcastListener(new ServiceBroadcastListener.EventHandler() {
                public void receivedServiceDescription(InetAddress address, ServiceDescription description) {
                    if (Extendo.VERBOSE) {
                        LOGGER.info("received broadcast message from " + address.getHostAddress()
                                + ": version=" + description.getVersion()
                                + ", endpoint=" + description.getEndpoint()
                                + ", pub/sub port=" + description.getPubsubPort());
                    }

                    // currently, the first broadcast message is used to discover the service,
                    // which is assumed to remain available indefinitely.
                    // Subsequent messages are received but disregarded.
                    if (null == service) {
                        service = new Service();
                        service.address = address;
                        service.description = description;

                        Socket socket;
                        try {
                            LOGGER.info("opening socket connection to facilitator");
                            socket = new Socket(address, service.description.getPubsubPort());
                        } catch (IOException e) {
                            LOGGER.severe("failed to open socket connection to facilitator: " + e.getMessage());
                            e.printStackTrace(System.err);
                            return;
                        }
                        facilitatorConnection.start(socket);
                    } else {
                        if (Extendo.VERBOSE) {
                            LOGGER.info("ignoring broadcast message due to existing service at " + service.address.getHostAddress());
                        }
                    }
                }
            });
            listener.start();
        }
    }

    public URI getAgentUri() {
        return agentUri;
    }

    public QueryEngine getQueryEngine() {
        return queryEngine;
    }

    public DatasetFactory getDatasetFactory() {
        return factory;
    }

    public void stop() {
        if (null != listener) {
            listener.stop();
        }
    }

    public void broadcastDataset(final Dataset d) throws RDFHandlerException, IOException {
        if (null == service) {
            LOGGER.info("can't broadcast dataset; no facilitator service has been discovered");
            return;
        }

        // TODO: these shouldn't be hard-coded
        String protocolPrefix = "http://";
        int rexsterPort = 8182;

        String ip = service.address.getHostAddress();

        String endpoint = protocolPrefix + ip + ":" + rexsterPort + service.description.getEndpoint() + "broadcast-rdf";

        if (Extendo.VERBOSE) {
            LOGGER.info("broadcasting RDF dataset to endpoint at " + endpoint);
        }

        //String endpoint = "http://localhost:8182/graphs/joshkb/extendo/broadcast-rdf";

        String content;

        if (false) {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            RDFWriter w = Rio.createWriter(RDFFormat.NTRIPLES, bos);
            w.startRDF();
            for (Statement st : d.getStatements()) {
                w.handleStatement(st);
            }
            w.endRDF();
            content = bos.toString();
        } else {
            content = "<urn:uuid:426097f7-8840-49fe-a460-a234d62856fd> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://fortytwo.net/2013/extendo/gesture#GenericBatonGesture> .\n" +
                    "<urn:uuid:426097f7-8840-49fe-a460-a234d62856fd> <http://fortytwo.net/2013/extendo/gesture#expressedBy> <http://example.org/ns#bob> .\n" +
                    "<urn:uuid:ef191699-2663-4e4e-8c09-57b705f28d21> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/NET/c4dm/timeline.owl#Instant> .\n" +
                    "<urn:uuid:ef191699-2663-4e4e-8c09-57b705f28d21> <http://purl.org/NET/c4dm/timeline.owl#at> \"2013-11-25T07:06:35-0500\"^^<http://www.w3.org/2001/XMLSchema#dateTime> .\n" +
                    "<urn:uuid:426097f7-8840-49fe-a460-a234d62856fd> <http://fortytwo.net/2013/extendo/gesture#recognizedAt> <urn:uuid:ef191699-2663-4e4e-8c09-57b705f28d21> .\n";
        }

        HttpPost request = new HttpPost(endpoint);
        List<NameValuePair> params = new ArrayList<NameValuePair>();
        params.add(new BasicNameValuePair("dataset", content));
        request.setEntity(new UrlEncodedFormEntity(params));

        HttpResponse response = httpclient.execute(request);
        StatusLine statusLine = response.getStatusLine();
        if (statusLine.getStatusCode() == HttpStatus.SC_OK) {
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            response.getEntity().writeTo(out);
            out.close();
            String responseString = out.toString();
            //..more logic
            if (Extendo.VERBOSE) {
                LOGGER.info("successfully pushed gestural event RDF");
            }
        } else {
            //Closes the connection.
            response.getEntity().getContent().close();
            LOGGER.severe("failed to push gestural event RDF: " + statusLine.getReasonPhrase());
        }
    }

    private class Service {
        public InetAddress address;
        public ServiceDescription description;
    }
}
