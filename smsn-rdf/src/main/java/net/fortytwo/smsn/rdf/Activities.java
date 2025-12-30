package net.fortytwo.smsn.rdf;

import net.fortytwo.smsn.rdf.vocab.FOAF;
import net.fortytwo.smsn.rdf.vocab.SmSnActivityOntology;
import net.fortytwo.smsn.rdf.vocab.Timeline;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.model.vocabulary.XSD;

import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.LinkedList;

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

    private static final ValueFactory vf = RDF4JUtil.getValueFactory();

    private static final Resource[] defaultGraphArray = {null};

    /**
     * Creates an RDF dataset for a pointing event
     *
     * @param timestamp the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param actor     the person performing the action of pointing
     * @param referent  the thing referenced or physically pointed to
     * @param graphs    RDF graphs of the dataset
     * @return an RDF dataset describing the activity
     */
    public static RDFDataset datasetForPointingGesture(final long timestamp,
                                                    final Resource actor,
                                                    final Resource referent,
                                                    Resource... graphs) {
        if (null == actor || null == referent) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<>();
        IRI activity = RDF4JUtil.randomIRI();

        for (Resource graph : fixGraphs(graphs)) {
            c.add(createStatement(activity, RDF.TYPE, SmSnActivityOntology.Point, graph));
            c.add(createStatement(activity, SmSnActivityOntology.referent, referent, graph));
        }

        return datasetForGesture(timestamp, activity, c, actor, graphs);
    }

    /**
     * Creates an RDF dataset for a generic baton gesture
     *
     * @param timestamp the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param actor     the person performing the gesture
     * @param graphs    RDF graphs of the dataset
     * @return an RDF dataset describing the activity
     */
    public static RDFDataset datasetForBatonGesture(final long timestamp,
                                                 final Resource actor,
                                                 Resource... graphs) {
        if (null == actor) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<>();
        IRI activity = RDF4JUtil.randomIRI();

        for (Resource graph : fixGraphs(graphs)) {
            c.add(createStatement(activity, RDF.TYPE, SmSnActivityOntology.BatonGesture, graph));
        }

        return datasetForGesture(timestamp, activity, c, actor, graphs);
    }

    /**
     * Creates an RDF dataset for any of the basic up-and-down motions of a handshake
     *
     * @param timestamp the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param actor     the person performing the gesture
     * @param graphs    RDF graphs of the dataset
     * @return an RDF dataset describing the activity
     */
    public static RDFDataset datasetForHandshakePulse(final long timestamp,
                                                   final Resource actor,
                                                   Resource... graphs) {
        if (null == actor) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<>();
        IRI activity = RDF4JUtil.randomIRI();

        for (Resource graph : fixGraphs(graphs)) {
            c.add(createStatement(activity, RDF.TYPE, SmSnActivityOntology.HandshakePulse, graph));
        }

        return datasetForGesture(timestamp, activity, c, actor, graphs);
    }

    /**
     * Creates an RDF dataset for a handshake interaction
     *
     * @param timestamp the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param actor1    one of the two people shaking hands
     * @param actor2    one of the two people shaking hands
     * @param graphs    RDF graphs of the dataset
     * @return an RDF dataset describing the activity
     */
    public static RDFDataset datasetForHandshakeInteraction(final long timestamp,
                                                         final Resource actor1,
                                                         final Resource actor2,
                                                         Resource... graphs) {
        if (null == actor1 || null == actor2) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<>();
        IRI activity = RDF4JUtil.randomIRI();

        for (Resource graph : fixGraphs(graphs)) {
            c.add(createStatement(activity, RDF.TYPE, SmSnActivityOntology.Handshake, graph));
            c.add(createStatement(activity, SmSnActivityOntology.actor, actor1, graph));
            c.add(createStatement(activity, SmSnActivityOntology.actor, actor2, graph));
        }

        return datasetForActivity(timestamp, activity, c, graphs);
    }

    /**
     * Creates an RDF dataset for a hand-off interaction, during which one person physically or virtually
     * gives an item to the other
     *
     * @param timestamp  the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param giver      the person giving the item
     * @param taker      the person receiving the item
     * @param thingGiven the item given
     * @param graphs    RDF graphs of the dataset
     * @return an RDF dataset describing the activity
     */
    public static RDFDataset datasetForHandoffInteraction(final long timestamp,
                                                       final Resource giver,
                                                       final Resource taker,
                                                       final Resource thingGiven,
                                                       Resource... graphs) {
        if (null == giver || null == taker || null == thingGiven) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<>();
        IRI activity = RDF4JUtil.randomIRI();

        for (Resource graph : fixGraphs(graphs)) {
            c.add(createStatement(activity, RDF.TYPE, SmSnActivityOntology.Handoff, graph));
            c.add(createStatement(activity, SmSnActivityOntology.giver, giver, graph));
            c.add(createStatement(activity, SmSnActivityOntology.taker, taker, graph));
            c.add(createStatement(activity, SmSnActivityOntology.thingGiven, thingGiven, graph));
        }

        return datasetForActivity(timestamp, activity, c, graphs);
    }

    /**
     * Creates an RDF dataset for a high-five interaction
     *
     * @param timestamp the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param actor1    one of the two people clapping hands
     * @param actor2    one of the two people clapping hands
     * @param graphs    RDF graphs of the dataset
     * @return an RDF dataset describing the activity
     */
    public static RDFDataset datasetForHighFiveInteraction(final long timestamp,
                                                        final Resource actor1,
                                                        final Resource actor2,
                                                        Resource... graphs) {
        if (null == actor1 || null == actor2) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<>();
        IRI activity = RDF4JUtil.randomIRI();

        for (Resource graph : fixGraphs(graphs)) {
            c.add(createStatement(activity, RDF.TYPE, SmSnActivityOntology.HighFive, graph));
            c.add(createStatement(activity, SmSnActivityOntology.actor, actor1, graph));
            c.add(createStatement(activity, SmSnActivityOntology.actor, actor2, graph));
        }

        return datasetForActivity(timestamp, activity, c, graphs);
    }

    /**
     * Creates an RDF dataset for the activity of attention to an item of interest
     *
     * @param timestamp        the moment at which the activity was recognized, in milliseconds since the Unix epoch
     * @param actor            the person attending to the item
     * @param focusOfAttention the object of attention
     * @param graphs    RDF graphs of the dataset
     * @return an RDF dataset describing the activity
     */
    public static RDFDataset datasetForAttentionActivity(final long timestamp,
                                                      final Resource actor,
                                                      final Resource focusOfAttention,
                                                      Resource... graphs) {
        if (null == actor || null == focusOfAttention) {
            throw new IllegalArgumentException();
        }

        Collection<Statement> c = new LinkedList<>();
        IRI activity = RDF4JUtil.randomIRI();

        for (Resource graph : fixGraphs(graphs)) {
            c.add(createStatement(activity, RDF.TYPE, SmSnActivityOntology.Attention, graph));
            c.add(createStatement(activity, SmSnActivityOntology.actor, actor, graph));
            c.add(createStatement(activity, SmSnActivityOntology.focusOfAttention, focusOfAttention, graph));
        }

        return datasetForActivity(timestamp, activity, c, graphs);
    }

    private static RDFDataset datasetForGesture(final long timestamp,
                                             final Resource activity,
                                             final Collection<Statement> c,
                                             final Resource agent,
                                             Resource... graphs) {
        for (Resource graph : fixGraphs(graphs)) {
            c.add(createStatement(activity, SmSnActivityOntology.actor, agent, graph));
        }

        return datasetForActivity(timestamp, activity, c, graphs);
    }

    private static RDFDataset datasetForActivity(final long timestamp,
                                              final Resource activity,
                                              final Collection<Statement> c,
                                              Resource... graphs) {
        for (Resource graph : fixGraphs(graphs)) {
            IRI instant = RDF4JUtil.randomIRI();
            c.add(createStatement(instant, RDF.TYPE, Timeline.Instant, graph));
            Literal dateValue = vf.createLiteral(TIMESTAMP_FORMAT.format(timestamp), XSD.DATETIME);
            c.add(createStatement(instant, Timeline.at, dateValue, graph));
            c.add(createStatement(activity, SmSnActivityOntology.recognitionTime, instant, graph));
        }

        return new RDFDataset(c);
    }

    private static Resource[] fixGraphs(Resource... graphs) {
        return (0 == graphs.length) ? defaultGraphArray : graphs;
    }

    private static Statement createStatement(Resource subject, IRI predicate, Value object, Resource graph) {
        return vf.createStatement(subject, predicate, object, graph);
    }
}
