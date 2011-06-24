package net.fortytwo.myotherbrain.notes;

import com.tinkerpop.blueprints.pgm.CloseableSequence;
import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Element;
import com.tinkerpop.blueprints.pgm.Index;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.blueprints.pgm.impls.tg.TinkerGraph;
import com.tinkerpop.frames.FramesManager;
import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.MyOtherBrain;

import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

/**
 * User: josh
 * Date: 6/18/11
 * Time: 7:26 PM
 */
public class NotesSemantics {
    private static final String KEYS = "keys";

    private static final Random RANDOM = new Random();

    private final IndexableGraph graph;
    private final FramesManager manager;
    private final Index<Vertex> keys;

    public NotesSemantics(final IndexableGraph graph,
                          final FramesManager manager) {
        this.graph = graph;
        this.manager = manager;

        // TODO: it would be more convenient if IndexableGraph would return a null (with getIndex) for a non-existent index, instead of throwing an exception
        boolean indexExists = false;
        for (Index<? extends Element> index : graph.getIndices()) {
            if (index.getIndexName().equals(KEYS)) {
                indexExists = true;
                break;
            }
        }

        if (!indexExists) {
            Set<String> keys = new HashSet<String>();
            keys.add(MyOtherBrain.KEY);
            graph.createAutomaticIndex(KEYS, Vertex.class, keys);
        }

        keys = graph.getIndex(KEYS, Vertex.class);
    }

    /**
     * Generates a view of the graph.
     *
     * @param root    the key of the root atom of the view
     * @param depth   the depth of the view.
     *                A view of depth 0 contains only the root,
     *                while a view of depth 1 also contains all children of the root,
     *                a view of depth 2 all grandchildren, etc.
     * @param filter  a collection of criteria for atoms and links.
     *                Atoms and links which do not meet the criteria are not to appear in the view.
     * @param inverse whether to produce an inverted view (in which links are followed "backwards")
     * @return a partial view of the graph as a tree of <code>Note</code> objects
     */
    public Note view(final Atom root,
                     final int depth,
                     final Filter filter,
                     final boolean inverse) {

        String type = root.getType();
        String value = root.getValue();
        Note n = new Note(type, value);
        n.setTargetKey(root.getKey());

        if (depth > 0) {
            Collection<Atom> links = inverse ? getInLinks(root, filter) : getOutlinks(root, filter);
            for (Atom link : links) {
                Atom target = inverse ? link.getFrom() : link.getTo();

                if (null == target) {
                    throw new IllegalArgumentException("link " + link.getKey() + " has no '" + (inverse ? "from" : "to") + "' atom");
                }

                Note n2 = view(target, depth - 1, filter, inverse);
                n2.setLinkKey(link.getKey());
                n.addChild(n2);
            }
        }

        //System.out.println("yielding note: " + n);
        return n;
    }

    /**
     * Updates the graph.
     *
     * @param root     the root of the subgraph to be updated
     * @param children the children of the root atom
     * @param depth    the minimum depth to which the graph will be updated
     * @param filter   a collection of criteria for atoms and links.
     *                 Atoms and links which do not meet the criteria are not to be affected by the update.
     * @param inverse  whether to push an inverted view (in which links are followed "backwards")
     * @throws InvalidUpdateException if the update cannot be performed as specified
     */
    public void update(final Atom root,
                       final List<Note> children,
                       final int depth,
                       final Filter filter,
                       final boolean inverse) throws InvalidUpdateException {
        updateInternal(root, children, depth, filter, true, inverse);
    }

    private void updateInternal(final Atom root,
                                final List<Note> children,
                                final int depth,
                                final Filter filter,
                                boolean destructive,
                                boolean inverse) throws InvalidUpdateException {
        if (depth < 1) {
            destructive = false;
        }

        List<Note> before = view(root, 1, filter, inverse).getChildren();

        Map<String, Note> beforeMap = new HashMap<String, Note>();
        for (Note n : before) {
            beforeMap.put(n.getLinkKey(), n);
        }

        Map<String, Note> afterMap = new HashMap<String, Note>();
        for (Note n : children) {
            if (null != n.getLinkKey()) {
                afterMap.put(n.getLinkKey(), n);
            }
        }

        // Remove any deleted links
        if (destructive) {
            for (String linkKey : beforeMap.keySet()) {
                if (afterMap.keySet().contains(linkKey)) {
                    Note b = beforeMap.get(linkKey);
                    Note a = afterMap.get(linkKey);
                    if (null == a.getTargetKey()) {
                        throw new InvalidUpdateException("non-null link key with null target key");
                    } else if (!a.getTargetKey().equals(b.getTargetKey())) {
                        throw new InvalidUpdateException("target key of updated link has changed");
                    }
                } else {
                    // Avoid attempting to remove a link more than once (if it appears more than once in the tree).
                    beforeMap.remove(linkKey);

                    breakLink(getAtom(linkKey));
                }
            }
        }

        // Add any new links, and update fields
        for (Note n : children) {
            Atom target;
            if (null == n.getTargetKey()) {
                target = createAtom(filter);
            } else {
                target = getAtom(n.getTargetKey());
                if (null == target) {
                    throw new InvalidUpdateException("no such atom: " + n.getTargetKey());
                }

                // Note: if we were to equate visibility with security, this would be a security issue,
                // as one could simply guess keys randomly until one finds an actual atom, making it visible.
                // The benefit of visibility is for user interaction, not access control.
                filter.makeVisible(target);
            }
            target.setType(n.getType());
            target.setValue(n.getValue());

            String linkKey = n.getLinkKey();
            boolean createLink = false;

            if (null == linkKey) {
                createLink = true;
                destructive = false;
            } else if (null == beforeMap.get(linkKey)) {
                Atom link = getAtom(linkKey);
                boolean exists = link != null;
                if (exists) {
                    exists = inverse
                            ? link.getFrom().getKey().equals(target.getKey()) && link.getTo().getKey().equals(root.getKey())
                            : link.getFrom().getKey().equals(root.getKey()) && link.getTo().getKey().equals(target.getKey());
                }

                if (exists) {
                    filter.makeVisible(link);
                } else {
                    createLink = true;
                }

                destructive = false;
            } else {
                // Validate against the existing link
                if (null == n.getTargetKey()) {
                    throw new InvalidUpdateException("non-null link key with null target key");
                } else if (!n.getTargetKey().equals(beforeMap.get(linkKey).getTargetKey())) {
                    throw new InvalidUpdateException("target key of updated link has changed");
                }
            }

            if (createLink) {
                Atom link = createAtom(filter);
                if (inverse) {
                    link.setFrom(target);
                    link.setTo(root);
                } else {
                    link.setFrom(root);
                    link.setTo(target);
                }
            }

            updateInternal(target, n.getChildren(), depth - 1, filter, destructive, inverse);
        }
    }

    private void breakLink(final Atom link) {
        link.setFrom(null);
        link.setTo(null);
    }

    private Atom getAtom(final Vertex v) {
        return manager.frame(v, Atom.class);
    }

    public Atom getAtom(final String key) throws InvalidUpdateException {
        if (null == key) {
            throw new IllegalArgumentException("null atom key");
        }

        Vertex v;

        CloseableSequence<Vertex> s = keys.get(MyOtherBrain.KEY, key);
        try {
            if (!s.hasNext()) {
                return null;
            }
            v = s.next();
            if (s.hasNext()) {
                throw new IllegalStateException("multiple vertices with the same key: '" + key + "'");
            }
        } finally {
            s.close();
        }

        return getAtom(v);
    }

    private Atom createAtom(final Filter filter) {
        Atom a = manager.frame(graph.addVertex(null), Atom.class);
        a.setKey(createKey());
        a.setCreated(new Date().getTime());

        // Place visibility and weight in the middle of the applicable ranges.
        a.setVisibility((filter.maxVisibility - filter.minVisibility) / 2);
        a.setWeight((filter.maxWeight - filter.minWeight) / 2);

        return a;
    }

    private String createKey() {
        byte[] bytes = new byte[5];
        for (int i = 0; i < 5; i++) {
            int n = RANDOM.nextInt(64);
            int b = n < 26
                    ? 'A' + n
                    : n < 52
                    ? 'a' + n - 26
                    : n < 62
                    ? '0' + n - 52
                    : n < 63
                    ? '/' : '+';
            bytes[i] = (byte) b;
        }

        return new String(bytes);
    }

    private Collection<Atom> getInLinks(final Atom from,
                                        final Filter filter) {
        List<TimestampedAtom> c = new LinkedList<TimestampedAtom>();

        for (Edge e : from.asVertex().getInEdges(MyOtherBrain.TO)) {
            Atom link = getAtom(e.getOutVertex());
            if (filter.isVisible(link) && filter.isVisible(link.getFrom())) {
                c.add(new TimestampedAtom(link));
            }
        }

        Collections.sort(c);

        Collection<Atom> r = new LinkedList<Atom>();
        for (TimestampedAtom ta : c) {
            r.add(ta.atom);
        }

        return r;
    }

    private Collection<Atom> getOutlinks(final Atom from,
                                         final Filter filter) {
        List<TimestampedAtom> c = new LinkedList<TimestampedAtom>();

        for (Edge e : from.asVertex().getInEdges(MyOtherBrain.FROM)) {
            Atom link = getAtom(e.getOutVertex());
            if (filter.isVisible(link) && filter.isVisible(link.getTo())) {
                c.add(new TimestampedAtom(link));
            }
        }

        Collections.sort(c);

        Collection<Atom> r = new LinkedList<Atom>();
        for (TimestampedAtom ta : c) {
            r.add(ta.atom);
        }

        return r;
    }

    private class TimestampedAtom implements Comparable<TimestampedAtom> {
        public Atom atom;
        public long timestamp;

        public TimestampedAtom(final Atom a) {
            atom = a;
            timestamp = a.getCreated();
        }

        // Order from newest to oldest
        public int compareTo(final TimestampedAtom other) {
            return ((Long) other.timestamp).compareTo(timestamp);
        }
    }

    public static class InvalidUpdateException extends Exception {
        public InvalidUpdateException(final String message) {
            super(message);
        }
    }

    public static void main(final String[] args) throws Exception {
        Filter filter = new Filter(0, 1, 0, 1);

        NotesSyntax p = new NotesSyntax();
        List<Note> notes;

        //InputStream in = new FileInputStream("/tmp/notes.txt");
        InputStream in = new FileInputStream("/Users/josh/notes/notes.txt");
        try {
            notes = p.flatten(p.parseContexts(in));
        } finally {
            in.close();
        }

        IndexableGraph graph = new TinkerGraph();
        FramesManager manager = new FramesManager(graph);
        NotesSemantics m = new NotesSemantics(graph, manager);
        Atom root = m.createAtom(filter);
        root.asVertex().setProperty(MyOtherBrain.KEY, "00000");
        root.setValue("Josh's notes");
        root.setType(".");
        m.update(root, notes, 0, filter, false);

        //GraphMLWriter.outputGraph(graph, System.out);
        //System.out.println();

        Note n = m.view(root, 3, filter, false);
        p.writeChildren(n, System.out);
    }
}
