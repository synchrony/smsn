package net.fortytwo.extendo.brainstem;

import android.util.Log;
import net.fortytwo.extendo.rdf.vocab.ExtendoGesture;
import net.fortytwo.extendo.rdf.vocab.Timeline;
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
import org.openrdf.model.Literal;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.model.vocabulary.XMLSchema;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFWriter;
import org.openrdf.rio.Rio;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BrainstemAgent {

    private static final SimpleDateFormat XSD_DATETIME_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");

    private final URI agentUri;
    private final ValueFactory vf;

    private final DatasetFactory factory;

    private final String broadcastRdfEndpointUrl;

    private final HttpClient httpclient = new DefaultHttpClient();

    public static final String QUERY_FOR_ALL_GB_GESTURES =
            "PREFIX gesture: <" + ExtendoGesture.NAMESPACE + ">\n" +
                    "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
                    "SELECT ?person ?time WHERE {\n" +
                    "?gesture a gesture:GenericBatonGesture .\n" +
                    "?gesture gesture:expressedBy ?person .\n" +
                    "?gesture gesture:recognizedAt ?instant .\n" +
                    "?instant tl:at ?time .\n" +
                    "}";

    public BrainstemAgent(final String agentUri,
                          final Properties config,
                          final String broadcastRdfEndpointUrl) throws IOException {
        factory = new DatasetFactory();
        vf = factory.getValueFactory();
        this.agentUri = vf.createURI(agentUri);
        this.broadcastRdfEndpointUrl = broadcastRdfEndpointUrl;
    }

    public URI getAgentUri() {
        return agentUri;
    }

    public DatasetFactory getDatasetFactory() {
        return factory;
    }

    /**
     * @param timestamp the moment at which the gesture was recognized, in milliseconds since the Unix epoch
     * @return a Dataset describing the gesture event
     */
    public Dataset datasetForGestureEvent(final long timestamp) {
        Collection<Statement> c = new LinkedList<Statement>();

        URI gesture = factory.randomURI();
        c.add(vf.createStatement(gesture, RDF.TYPE, ExtendoGesture.GenericBatonGesture));

        c.add(vf.createStatement(gesture, ExtendoGesture.expressedBy, agentUri));
        //c.add(vf.createStatement(selfUri, RDF.TYPE, FOAF.AGENT));

        URI instant = factory.randomURI();
        c.add(vf.createStatement(instant, RDF.TYPE, Timeline.Instant));
        Literal dateValue = vf.createLiteral(XSD_DATETIME_FORMAT.format(new Date(timestamp)), XMLSchema.DATETIME);
        c.add(vf.createStatement(instant, Timeline.at, dateValue));
        c.add(vf.createStatement(gesture, ExtendoGesture.recognizedAt, instant));

        return new Dataset(c);
    }

    public void sendDataset(final Dataset d) throws RDFHandlerException, IOException {
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

        HttpPost request = new HttpPost(broadcastRdfEndpointUrl);
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
            Log.i(Brainstem.TAG, "successfully pushed gestural event RDF");
        } else {
            //Closes the connection.
            response.getEntity().getContent().close();
            Log.e(Brainstem.TAG, "failed to push gestural event RDF: " + statusLine.getReasonPhrase());
        }
    }

}
