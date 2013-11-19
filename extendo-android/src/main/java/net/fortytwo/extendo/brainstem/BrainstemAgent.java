package net.fortytwo.extendo.brainstem;

import net.fortytwo.extendo.rdf.vocab.ExtendoGesture;
import net.fortytwo.extendo.rdf.vocab.Timeline;
import net.fortytwo.rdfagents.data.DatasetFactory;
import net.fortytwo.rdfagents.model.Dataset;
import org.openrdf.model.Literal;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.model.vocabulary.XMLSchema;

import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BrainstemAgent {

    private static final SimpleDateFormat XSD_DATETIME_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");

    private final URI agentUri;
    private final ValueFactory vf;

    private final DatasetFactory factory;

    public static final String QUERY_FOR_ALL_GB_GESTURES =
            "PREFIX gesture: <" + ExtendoGesture.NAMESPACE + ">\n" +
                    "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
                    "SELECT ?person ?time WHERE {\n" +
                    "?gesture a gesture:GenericBatonGesture .\n" +
                    "?gesture gesture:expressedBy ?person .\n" +
                    "?gesture gesture:recognizedAt ?instant .\n" +
                    "?instant tl:at ?time .\n" +
                    "}";

    public BrainstemAgent(final String agentUri) {
        factory = new DatasetFactory();
        vf = factory.getValueFactory();
        this.agentUri = vf.createURI(agentUri);
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
}
