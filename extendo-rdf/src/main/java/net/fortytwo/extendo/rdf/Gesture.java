package net.fortytwo.extendo.rdf;

import net.fortytwo.extendo.rdf.vocab.ExtendoGesture;
import net.fortytwo.extendo.rdf.vocab.FOAF;
import net.fortytwo.extendo.rdf.vocab.Timeline;
import net.fortytwo.rdfagents.data.DatasetFactory;
import net.fortytwo.rdfagents.model.Dataset;
import org.openrdf.model.Literal;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.model.vocabulary.XMLSchema;

import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Gesture {
    private static final SimpleDateFormat XSD_DATETIME_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");

    public static final String QUERY_FOR_ALL_GB_GESTURES =
            "PREFIX gesture: <" + ExtendoGesture.NAMESPACE + ">\n" +
                    "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
                    "SELECT ?person ?time WHERE {\n" +
                    "?gesture a gesture:GenericBatonGesture .\n" +
                    "?gesture gesture:expressedBy ?person .\n" +
                    "?gesture gesture:recognizedAt ?instant .\n" +
                    "?instant tl:at ?time .\n" +
                    "}";
    public static final String QUERY_FOR_THING_POINTED_TO =
            "PREFIX gesture: <" + ExtendoGesture.NAMESPACE + ">\n" +
                    "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
                    "PREFIX foaf: <" + FOAF.NAMESPACE + ">\n" +
                    "PREFIX rdfs: <" + RDFS.NAMESPACE + ">\n" +
                    "SELECT ?person ?pointedTo WHERE {\n" +
                    "?gesture gesture:thingPointedTo ?pointedTo .\n" +
                    "?gesture gesture:expressedBy ?person .\n" +
                    "}";
    public static final String QUERY_FOR_POINT_WITH_COMMON_ORG =
            "PREFIX gesture: <" + ExtendoGesture.NAMESPACE + ">\n" +
                    "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
                    "PREFIX foaf: <" + FOAF.NAMESPACE + ">\n" +
                    "PREFIX rdfs: <" + RDFS.NAMESPACE + ">\n" +
                    "SELECT ?personPointedTo ?personPointedToName ?orgLabel WHERE {\n" +
                    "?gesture gesture:thingPointedTo ?personPointedTo .\n" +
                    "?personPointedTo foaf:name ?personPointedToName .\n" +
                    "?org rdfs:label ?orgLabel .\n" +
                    "?org foaf:member <http://fortytwo.net/2014/04/twc#JoshuaShinavier> .\n" +
                    "?org foaf:member ?personPointedTo .\n" +
                    "}";
    public static final String QUERY_FOR_POINT_WITH_COMMON_INTEREST =
            "PREFIX gesture: <" + ExtendoGesture.NAMESPACE + ">\n" +
                    "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
                    "PREFIX foaf: <" + FOAF.NAMESPACE + ">\n" +
                    "PREFIX rdfs: <" + RDFS.NAMESPACE + ">\n" +
                    "SELECT ?personPointedTo ?personPointedToName ?interest WHERE {\n" +
                    "?personPointedTo foaf:name ?personPointedToName .\n" +
                    "?gesture gesture:thingPointedTo ?personPointedTo .\n" +
                    "<http://fortytwo.net/2014/04/twc#JoshuaShinavier> foaf:interest ?interest .\n" +
                    "?personPointedTo foaf:interest ?interest .\n" +
                    "}";

    private static final DatasetFactory factory = new DatasetFactory();
    private static final ValueFactory vf = factory.getValueFactory();

    /**
     * Creates a dataset for a pointing event
     *
     * @param timestamp      the moment at which the gesture was recognized, in milliseconds since the Unix epoch
     * @param thingPointedTo the thing which was referenced or physically pointed to by the gesture
     * @return a Dataset describing the gesture event
     */
    public static Dataset datasetForPointingGesture(final long timestamp,
                                             final URI agentUri,
                                             final URI thingPointedTo) {
        return datasetForGesture(timestamp, agentUri, thingPointedTo);
    }

    /**
     * Creates a dataset for a generic baton event
     *
     * @param timestamp the moment at which the gesture was recognized, in milliseconds since the Unix epoch
     * @return a Dataset describing the gesture event
     */
    public static Dataset datasetForGenericBatonGesture(final long timestamp,
                                                 final URI agentUri) {
        return datasetForGesture(timestamp, agentUri, null);
    }

    private static Dataset datasetForGesture(final long timestamp,
                                      final URI agentUri,
                                      final URI thingPointedTo) {
        Collection<Statement> c = new LinkedList<Statement>();

        URI gesture = factory.randomURI();

        if (null == thingPointedTo) {
            c.add(vf.createStatement(gesture, RDF.TYPE, ExtendoGesture.GenericBatonGesture));
        } else {
            c.add(vf.createStatement(gesture, RDF.TYPE, ExtendoGesture.Point));
            c.add(vf.createStatement(gesture, ExtendoGesture.thingPointedTo, thingPointedTo));
        }

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
