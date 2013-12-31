package net.fortytwo.extendo.p2p;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.methods.PostMethod;
import org.json.JSONException;
import org.json.JSONObject;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFWriter;
import org.openrdf.rio.Rio;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExampleBroadcaster {

    private void sendExampleDataset() throws RDFHandlerException, IOException, JSONException {
        String endpoint = "http://localhost:8182/graphs/joshkb/extendo/broadcast-rdf";

        String content;

        if (false) {
            ValueFactory vf = new ValueFactoryImpl();
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            RDFWriter w = Rio.createWriter(RDFFormat.NTRIPLES, bos);
            w.startRDF();
            w.handleStatement(vf.createStatement(RDF.TYPE, RDFS.LABEL, vf.createLiteral("type")));
            w.handleStatement(vf.createStatement(RDF.TYPE, RDFS.CLASS, RDF.PROPERTY));
            w.endRDF();
            content = bos.toString();
        } else {
            content = "<urn:uuid:426097f7-8840-49fe-a460-a234d62856fd> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://fortytwo.net/2013/extendo/gesture#GenericBatonGesture> .\n" +
                    "<urn:uuid:426097f7-8840-49fe-a460-a234d62856fd> <http://fortytwo.net/2013/extendo/gesture#expressedBy> <http://example.org/ns#bob> .\n" +
                    "<urn:uuid:ef191699-2663-4e4e-8c09-57b705f28d21> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/NET/c4dm/timeline.owl#Instant> .\n" +
                    "<urn:uuid:ef191699-2663-4e4e-8c09-57b705f28d21> <http://purl.org/NET/c4dm/timeline.owl#at> \"2013-11-25T07:06:35-0500\"^^<http://www.w3.org/2001/XMLSchema#dateTime> .\n" +
                    "<urn:uuid:426097f7-8840-49fe-a460-a234d62856fd> <http://fortytwo.net/2013/extendo/gesture#recognizedAt> <urn:uuid:ef191699-2663-4e4e-8c09-57b705f28d21> .\n";
        }

        HttpClient client = new HttpClient();

        PostMethod method = new PostMethod(endpoint);

        //method.addRequestHeader("Content-Type", "application/json");

        JSONObject r = new JSONObject();
        r.put("dataset", content);

        method.addParameter("request", r.toString());
        client.executeMethod(method);
        String responseBody = method.getResponseBodyAsString();
        System.out.println("response body: " + responseBody);
        method.releaseConnection();
    }

    public static void main(final String[] args) throws Exception {
        new ExampleBroadcaster().sendExampleDataset();
    }
}
