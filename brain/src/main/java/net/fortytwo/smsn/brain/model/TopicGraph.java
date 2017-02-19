package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.model.entities.KeyValueTree;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;

import java.io.IOException;
import java.util.List;

/**
 * A graph of atoms and lists conforming to the Extend-o-Brain data model
 */
public interface TopicGraph {

    /**
     * The configurable namespace into which things, i.e. classified atoms, are mapped
     */
    static final String PROP_THING_NAMESPACE = "net.fortytwo.smsn.brain.thingNamespace";

    static final String DEFAULT_THING_NAMESPACE = "http://example.org/things/";

    Iterable<Atom> getAllAtoms();

    Atom getAtomById(String id);

    List<Atom> getAtomsByAcronym(String acronym, Filter filter);

    List<Atom> getAtomsByShortcut(String shortcut, Filter filter);

    List<Atom> getAtomsByTitleQuery(String value, Filter filter);

    String idOfAtom(Atom a);

    String iriOfAtom(Atom a);

    Topic createTopic(String id);

    Page createPage(Link topicLink);

    Link createLink(Topic target, String label);

    KeyValueTree<Link, EntityList<Link>> createTopicTree(Link link);

    Atom createAtom(String id);

    Atom createAtomWithProperties(Filter filter, String id);

    EntityList<Link> createListOfLinks(Link... elements);

    EntityList<KeyValueTree<Link, EntityList<Link>>> createListOfTrees(
            KeyValueTree<Link, EntityList<Link>>... elements);

    EntityList<Atom> createListOfAtoms(Atom... elements);

    void removeIsolatedAtoms(Filter filter);

    void notifyOfUpdate();

    void reindexAtom(Atom a);

    long getLastUpdate();

    void begin();

    void commit();

    void rollback();

    TopicGraph createFilteredGraph(Filter filter);

    interface IORunnable {
        void run() throws IOException;
    }

    static void wrapInTransaction(final TopicGraph graph, final IORunnable runnable) throws IOException {
        graph.begin();

        boolean success = false;
        try {
            runnable.run();
            success = true;
        } finally {
            if (success) {
                graph.commit();
            } else {
                graph.rollback();
            }
        }
    }
}
