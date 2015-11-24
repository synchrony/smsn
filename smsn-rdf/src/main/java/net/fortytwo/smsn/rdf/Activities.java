package net.fortytwo.smsn.rdf;

import net.fortytwo.smsn.rdf.vocab.SmSnActivityOntology;
import net.fortytwo.smsn.rdf.vocab.FOAF;
import net.fortytwo.smsn.rdf.vocab.Timeline;
import net.fortytwo.rdfagents.data.DatasetFactory;
import net.fortytwo.rdfagents.model.Dataset;
import org.openrdf.model.Literal;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.model.vocabulary.XMLSchema;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFWriter;
import org.openrdf.rio.ntriples.NTriplesWriter;

import java.io.IOException;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Activities {
    /**
     * A XML Schema dateTime formatter for millisecond-precision timestamps
     */
    public static final SimpleDateFormat TIMESTAMP_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");

    public static final String QUERY_FOR_ALL_GB_GESTURES =
            "PREFIX activity: <" + SmSnActivityOntology.NAMESPACE + ">\n" +
                    "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
                    "SELECT ?actor ?time WHERE {\n" +
                    "?a a activity:BatonGesture .\n" +
                    "?a activity:actor ?actor .\n" +
                    "?a activity:recognitionTime ?instant .\n" +
                    "?instant tl:at ?time .\n" +
                    "}";
    public static final String QUERY_FOR_ATTENTION =
            "PREFIX activity: <" + SmSnActivityOntology.NAMESPACE + ">\n" +
                    "SELECT ?actor ?focus WHERE {\n" +
                    "?a activity:focusOfAttention ?focus .\n" +
                    "?a activity:actor ?actor .\n" +
                    "}";
    public static final String QUERY_FOR_REFERENTS =
            "PREFIX activity: <" + SmSnActivityOntology.NAMESPACE + ">\n" +
                    "SELECT ?actor ?referent WHERE {\n" +
                    "?a activity:referent ?referent .\n" +
                    "?a activity:actor ?actor .\n" +
                    "}";
    public static final String QUERY_FOR_REFERENTS_WITH_COMMON_ORG =
            "PREFIX activity: <" + SmSnActivityOntology.NAMESPACE + ">\n" +
                    "PREFIX foaf: <" + FOAF.NAMESPACE + ">\n" +
                    "PREFIX rdfs: <" + RDFS.NAMESPACE + ">\n" +
                    "SELECT ?referent ?referentName ?orgLabel WHERE {\n" +
                    "?a activity:referent ?referent .\n" +
                    "?referent foaf:name ?referentName .\n" +
                    "?org rdfs:label ?orgLabel .\n" +
                    "?org foaf:member <http://fortytwo.net/2014/04/twc#JoshuaShinavier> .\n" +  // TODO
                    "?org foaf:member ?referent .\n" +
                    "}";
    public static final String QUERY_FOR_REFERENTS_WITH_COMMON_INTEREST =
            "PREFIX activity: <" + SmSnActivityOntology.NAMESPACE + ">\n" +
                    "PREFIX foaf: <" + FOAF.NAMESPACE + ">\n" +
                    "SELECT ?referent ?referentName ?interest WHERE {\n" +
                    "?referent foaf:name ?referentName .\n" +
                    "?a activity:referent ?referent .\n" +
                    "<http://fortytwo.net/2014/04/twc#JoshuaShinavier> foaf:interest ?interest .\n" +  // TODO
                    "?referent foaf:interest ?interest .\n" +
                    "}";

    private static final DatasetFactory factory = new DatasetFactory();
    private static final ValueFactory vf = factory.getValueFactory();

    /**
     * Creates an RDF dataset for a pointing event
     *
     * @param timestamp      the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param actor          the person performing the action of pointing
     * @param referent the thing referenced or physically pointed to
     * @return an RDF dataset describing the activity
     */
    public static Dataset datasetForPointingGesture(final long timestamp,
                                                    final Resource actor,
                                                    final Resource referent) {
        if (null == actor || null == referent) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, SmSnActivityOntology.Point));
        c.add(vf.createStatement(activity, SmSnActivityOntology.referent, referent));

        return datasetForGesture(timestamp, activity, c, actor);
    }

    /**
     * Creates an RDF dataset for a generic baton gesture
     *
     * @param timestamp the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param actor     the person performing the gesture
     * @return an RDF dataset describing the activity
     */
    public static Dataset datasetForBatonGesture(final long timestamp,
                                                 final Resource actor) {
        if (null == actor) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, SmSnActivityOntology.BatonGesture));

        return datasetForGesture(timestamp, activity, c, actor);
    }

    /**
     * Creates an RDF dataset for any of the basic up-and-down motions of a handshake
     *
     * @param timestamp the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param actor     the person performing the gesture
     * @return an RDF dataset describing the activity
     */
    public static Dataset datasetForHandshakePulse(final long timestamp,
                                                   final Resource actor) {
        if (null == actor) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, SmSnActivityOntology.HandshakePulse));

        return datasetForGesture(timestamp, activity, c, actor);
    }

    /**
     * Creates an RDF dataset for a handshake interaction
     *
     * @param timestamp the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param actor1    one of the two people shaking hands
     * @param actor2    one of the two people shaking hands
     * @return an RDF dataset describing the activity
     */
    public static Dataset datasetForHandshakeInteraction(final long timestamp,
                                                         final Resource actor1,
                                                         final Resource actor2) {
        if (null == actor1 || null == actor2) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, SmSnActivityOntology.Handshake));
        c.add(vf.createStatement(activity, SmSnActivityOntology.actor, actor1));
        c.add(vf.createStatement(activity, SmSnActivityOntology.actor, actor2));

        return datasetForActivity(timestamp, activity, c);
    }

    /**
     * Creates an RDF dataset for a hand-off interaction, during which one person physically or virtually
     * gives an item to the other
     *
     * @param timestamp  the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param giver      the person giving the item
     * @param taker      the person receiving the item
     * @param thingGiven the item given
     * @return an RDF dataset describing the activity
     */
    public static Dataset datasetForHandoffInteraction(final long timestamp,
                                                       final Resource giver,
                                                       final Resource taker,
                                                       final Resource thingGiven) {
        if (null == giver || null == taker || null == thingGiven) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, SmSnActivityOntology.Handoff));
        c.add(vf.createStatement(activity, SmSnActivityOntology.giver, giver));
        c.add(vf.createStatement(activity, SmSnActivityOntology.taker, taker));
        c.add(vf.createStatement(activity, SmSnActivityOntology.thingGiven, thingGiven));

        return datasetForActivity(timestamp, activity, c);
    }

    /**
     * Creates an RDF dataset for a high-five interaction
     *
     * @param timestamp the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param actor1    one of the two people clapping hands
     * @param actor2    one of the two people clapping hands
     * @return an RDF dataset describing the activity
     */
    public static Dataset datasetForHighFiveInteraction(final long timestamp,
                                                        final Resource actor1,
                                                        final Resource actor2) {
        if (null == actor1 || null == actor2) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, SmSnActivityOntology.HighFive));
        c.add(vf.createStatement(activity, SmSnActivityOntology.actor, actor1));
        c.add(vf.createStatement(activity, SmSnActivityOntology.actor, actor2));

        return datasetForActivity(timestamp, activity, c);
    }

    /**
     * Creates an RDF dataset for the activity of attention to an item of interest
     *
     * @param timestamp        the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param actor            the person attending to the item
     * @param focusOfAttention the object of attention
     * @return an RDF dataset describing the activity
     */
    public static Dataset datasetForAttentionActivity(final long timestamp,
                                                      final Resource actor,
                                                      final Resource focusOfAttention) {
        if (null == actor || null == focusOfAttention) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, SmSnActivityOntology.Attention));
        c.add(vf.createStatement(activity, SmSnActivityOntology.actor, actor));
        c.add(vf.createStatement(activity, SmSnActivityOntology.focusOfAttention, focusOfAttention));

        return datasetForActivity(timestamp, activity, c);
    }

    private static Dataset datasetForGesture(final long timestamp,
                                             final Resource activity,
                                             final Collection<Statement> c,
                                             final Resource agent) {
        c.add(vf.createStatement(activity, SmSnActivityOntology.actor, agent));

        return datasetForActivity(timestamp, activity, c);
    }

    private static Dataset datasetForActivity(final long timestamp,
                                              final Resource activity,
                                              final Collection<Statement> c) {
        URI instant = factory.randomURI();
        c.add(vf.createStatement(instant, RDF.TYPE, Timeline.Instant));
        Literal dateValue = vf.createLiteral(TIMESTAMP_FORMAT.format(timestamp), XMLSchema.DATETIME);
        c.add(vf.createStatement(instant, Timeline.at, dateValue));
        c.add(vf.createStatement(activity, SmSnActivityOntology.recognitionTime, instant));

        return new Dataset(c);
    }

    public static void main(final String[] args) throws Exception {
        /*
        Dataset d = Activities.datasetForAttentionActivity(
                new Date().getTime(), new URIImpl("http://example.org/ArthurDent"), new URIImpl("http://example.org/sofa42"));
        *//*
        Dataset d = Activities.datasetForHandoffInteraction(new Date().getTime(),
                new URIImpl("http://fortytwo.net/josh/things/wRYn7qX"),
                new URIImpl("http://fortytwo.net/josh/things/E2jV2SK"),
                new URIImpl("http://fortytwo.net/josh/things/7N_7fqX"));
        */
        Dataset d = Activities.datasetForHandshakePulse(new Date().getTime(),
                new URIImpl("http://fortytwo.net/josh/things/CybU2QN"));

        OutputStream os = System.out;
        RDFWriter w = new NTriplesWriter(os);
        try {
            w.startRDF();
            for (Statement s : d.getStatements()) {
                w.handleStatement(s);
            }
            w.endRDF();
        } catch (RDFHandlerException e) {
            throw new IOException(e);
        }
    }
}
