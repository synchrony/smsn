package net.fortytwo.extendo.rdf;

import net.fortytwo.extendo.rdf.vocab.ExtendoActivityOntology;
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
public class Activities {
    private static final SimpleDateFormat XSD_DATETIME_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");

    public static final String QUERY_FOR_ALL_GB_GESTURES =
            "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
                    "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
                    "SELECT ?actor ?time WHERE {\n" +
                    "?a a activity:BatonGesture .\n" +
                    "?a activity:actor ?actor .\n" +
                    "?a activity:recognitionTime ?instant .\n" +
                    "?instant tl:at ?time .\n" +
                    "}";
    public static final String QUERY_FOR_ATTENTION =
            "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
                    "SELECT ?actor ?focus WHERE {\n" +
                    "?a activity:focusOfAttention ?focus .\n" +
                    "?a activity:actor ?actor .\n" +
                    "}";
    public static final String QUERY_FOR_THINGS_POINTED_TO =
            "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
                    "SELECT ?actor ?indicated WHERE {\n" +
                    "?a activity:thingIndicated ?indicated .\n" +
                    "?a activity:actor ?actor .\n" +
                    "}";
    public static final String QUERY_FOR_THINGS_POINTED_TO_WITH_COMMON_ORG =
            "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
                    "PREFIX foaf: <" + FOAF.NAMESPACE + ">\n" +
                    "PREFIX rdfs: <" + RDFS.NAMESPACE + ">\n" +
                    "SELECT ?indicated ?indicatedName ?orgLabel WHERE {\n" +
                    "?a activity:thingIndicated ?indicated .\n" +
                    "?indicated foaf:name ?indicatedName .\n" +
                    "?org rdfs:label ?orgLabel .\n" +
                    "?org foaf:member <http://fortytwo.net/2014/04/twc#JoshuaShinavier> .\n" +  // TODO
                    "?org foaf:member ?indicated .\n" +
                    "}";
    public static final String QUERY_FOR_THINGS_POINTED_TO_WITH_COMMON_INTEREST =
            "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
                    "PREFIX foaf: <" + FOAF.NAMESPACE + ">\n" +
                    "SELECT ?indicated ?indicatedName ?interest WHERE {\n" +
                    "?indicated foaf:name ?indicatedName .\n" +
                    "?a activity:thingIndicated ?indicated .\n" +
                    "<http://fortytwo.net/2014/04/twc#JoshuaShinavier> foaf:interest ?interest .\n" +  // TODO
                    "?indicated foaf:interest ?interest .\n" +
                    "}";

    private static final DatasetFactory factory = new DatasetFactory();
    private static final ValueFactory vf = factory.getValueFactory();

    /**
     * Creates an RDF dataset for a pointing event
     *
     * @param timestamp      the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param actor          the person performing the action of pointing
     * @param thingIndicated the thing referenced or physically pointed to
     * @return an RDF dataset describing the activity
     */
    public static Dataset datasetForPointingGesture(final long timestamp,
                                                    final URI actor,
                                                    final URI thingIndicated) {
        if (null == actor || null == thingIndicated) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, ExtendoActivityOntology.Point));
        c.add(vf.createStatement(activity, ExtendoActivityOntology.thingIndicated, thingIndicated));

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
                                                 final URI actor) {
        if (null == actor) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, ExtendoActivityOntology.BatonGesture));

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
                                                   final URI actor) {
        if (null == actor) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, ExtendoActivityOntology.HandshakePulse));

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
                                                         final URI actor1,
                                                         final URI actor2) {
        if (null == actor1 || null == actor2) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, ExtendoActivityOntology.Handshake));
        c.add(vf.createStatement(activity, ExtendoActivityOntology.actor, actor1));
        c.add(vf.createStatement(activity, ExtendoActivityOntology.actor, actor2));

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
                                                       final URI giver,
                                                       final URI taker,
                                                       final URI thingGiven) {
        if (null == giver || null == taker || null == thingGiven) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, ExtendoActivityOntology.Handoff));
        c.add(vf.createStatement(activity, ExtendoActivityOntology.giver, giver));
        c.add(vf.createStatement(activity, ExtendoActivityOntology.taker, taker));
        c.add(vf.createStatement(activity, ExtendoActivityOntology.thingGiven, thingGiven));

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
                                                        final URI actor1,
                                                        final URI actor2) {
        if (null == actor1 || null == actor2) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, ExtendoActivityOntology.HighFive));
        c.add(vf.createStatement(activity, ExtendoActivityOntology.actor, actor1));
        c.add(vf.createStatement(activity, ExtendoActivityOntology.actor, actor2));

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
                                                      final URI actor,
                                                      final URI focusOfAttention) {
        if (null == actor || null == focusOfAttention) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<Statement>();
        URI activity = factory.randomURI();

        c.add(vf.createStatement(activity, RDF.TYPE, ExtendoActivityOntology.Attention));
        c.add(vf.createStatement(activity, ExtendoActivityOntology.actor, actor));
        c.add(vf.createStatement(activity, ExtendoActivityOntology.focusOfAttention, focusOfAttention));

        return datasetForActivity(timestamp, activity, c);
    }

    private static Dataset datasetForGesture(final long timestamp,
                                             final URI activity,
                                             final Collection<Statement> c,
                                             final URI agentUri) {
        c.add(vf.createStatement(activity, ExtendoActivityOntology.actor, agentUri));

        return datasetForActivity(timestamp, activity, c);
    }

    private static Dataset datasetForActivity(final long timestamp,
                                              final URI activity,
                                              final Collection<Statement> c) {
        URI instant = factory.randomURI();
        c.add(vf.createStatement(instant, RDF.TYPE, Timeline.Instant));
        Literal dateValue = vf.createLiteral(XSD_DATETIME_FORMAT.format(new Date(timestamp)), XMLSchema.DATETIME);
        c.add(vf.createStatement(instant, Timeline.at, dateValue));
        c.add(vf.createStatement(activity, ExtendoActivityOntology.recognitionTime, instant));

        return new Dataset(c);
    }
}
