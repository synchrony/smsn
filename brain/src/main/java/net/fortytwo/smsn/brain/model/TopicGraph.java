package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

/**
 * A graph of atoms and lists conforming to the Extend-o-Brain data model
 */
public interface TopicGraph {

    Iterable<Atom> getAllAtoms();

    Optional<Atom> getAtomById(String id);

    List<Atom> getAtomsByAcronym(String acronym, Filter filter);

    List<Atom> getAtomsByShortcut(String shortcut, Filter filter);

    List<Atom> getAtomsByTitleQuery(String value, Filter filter);

    String idOfAtom(Atom a);

    String iriOfAtom(Atom a);

    Topic createTopic(String id);

    Page createPage(Link root);

    Link createLink(Topic target, String label, Role role);

    TreeNode<Link> createTopicTree(Link link);

    Atom createAtom(String id);

    Atom createAtomWithProperties(Filter filter, String id);

    ListNode<Link> createListOfLinks(Link... elements);

    ListNode<Topic> createListOfTopics(Topic... elements);

    ListNode<TreeNode<Link>> createListOfLinkTrees(TreeNode<Link>... elements);

    ListNode<Atom> createListOfAtoms(Atom... elements);

    void removeIsolatedAtoms(Filter filter);

    void notifyOfUpdate();

    void reindexAtom(Atom a);

    long getLastUpdate();

    void begin();

    void commit();

    void rollback();

    TopicGraph createFilteredGraph(Filter filter);

    interface IORunnable {
        void run();
    }

    static void wrapInTransaction(final TopicGraph graph, final IORunnable runnable) {
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
