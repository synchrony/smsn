package net.fortytwo.myotherbrain.notes;

import com.tinkerpop.blueprints.pgm.CloseableSequence;
import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Element;
import com.tinkerpop.blueprints.pgm.Index;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.blueprints.pgm.impls.tg.TinkerGraph;
import com.tinkerpop.frames.FramesManager;
import net.fortytwo.myotherbrain.MyOtherBrain;
import net.fortytwo.myotherbrain.model.frames.Atom;

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
public class NotesLens {
    private static final String KEYS = "keys";

    private static final Random RANDOM = new Random();

    private final IndexableGraph graph;
    private final FramesManager manager;
    private final Index<Vertex> keys;

    public NotesLens(final IndexableGraph graph,
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

    public Note view(final String atomKey,
                     final int depth) throws NoSuchRootException {
        Atom av;
        try {
            av = getAtom(atomKey);
        } catch (InvalidUpdateException e) {
            throw new NoSuchRootException("there is no atom with key '" + atomKey + "'");
        }
        String type = av.getType();
        String text = av.getText();
        Note n = new Note(type, text);
        n.setAtomKey(atomKey);

        if (depth > 0) {
            for (Atom ass : getOutboundAssociations(av)) {
                Atom to = ass.getTo();

                if (null == to) {
                    throw new IllegalArgumentException("association has no 'to' atom");
                }

                Note n2 = view(getKey(to), depth - 1);
                n2.setAssociationKey(getKey(ass));
                n.addChild(n2);
            }
        }

        //System.out.println("yielding note: " + n);
        return n;
    }

    public void update(final String rootKey,
                       final List<Note> children,
                       final int depth) throws InvalidUpdateException {
        update(getAtom(rootKey), children, depth, true);
    }

    private void update(final Atom root,
                        final List<Note> children,
                        final int depth,
                        boolean destructive) throws InvalidUpdateException {
        if (depth < 1) {
            destructive = false;
        }

        List<Note> before;
        try {
            before = view(getKey(root), 1).getChildren();
        } catch (NoSuchRootException e) {
            throw new InvalidUpdateException(e.getMessage());
        }

        Map<String, Note> beforeMap = new HashMap<String, Note>();
        for (Note n : before) {
            //System.out.println("\tbefore: " + n.getAssociationId() + ", " + n.getAtomId());
            beforeMap.put(n.getAssociationKey(), n);
        }

        Map<String, Note> afterMap = new HashMap<String, Note>();
        for (Note n : children) {
            if (null != n.getAssociationKey()) {
                //System.out.println("\tafter: " + n.getAssociationId() + ", " + n.getAtomId());
                afterMap.put(n.getAssociationKey(), n);
            }
        }

        // Remove any deleted associations
        for (String assId : beforeMap.keySet()) {
            if (afterMap.keySet().contains(assId)) {
                Note b = beforeMap.get(assId);
                Note a = afterMap.get(assId);
                if (null == a.getAtomKey()) {
                    throw new InvalidUpdateException("non-null association ID with null atom ID");
                } else if (!a.getAtomKey().equals(b.getAtomKey())) {
                    throw new InvalidUpdateException("atom ID of updated association has changed");
                }
            } else {
                System.out.println("breaking association " + assId);
                new Exception().printStackTrace();
                breakAssociation(getAtom(assId));
            }
        }

        // Add any new associations, and update fields
        for (Note n : children) {
            String assId = n.getAssociationKey();
            //System.out.println("assId = " + assId);
            Atom a = null == n.getAtomKey()
                    ? createAtom()
                    : getAtom(n.getAtomKey());

            root.setText(n.getText());
            root.setType(n.getType());

            if (null == assId || null == beforeMap.get(assId)) {
                destructive = false;

                Atom ass = createAtom();
                ass.setFrom(root);
                ass.setTo(a);
            } else {
                // Validate against the existing association
                if (null == n.getAtomKey()) {
                    throw new InvalidUpdateException("non-null association ID with null atom ID");
                } else if (!n.getAtomKey().equals(beforeMap.get(assId).getAtomKey())) {
                    throw new InvalidUpdateException("atom ID of updated association has changed");
                }
            }

            update(a, n.getChildren(), depth - 1, destructive);
        }
    }

    private void breakAssociation(final Atom ass) {
        ass.setFrom(null);
        ass.setTo(null);
    }

    private String getKey(final Atom a) {
        return (String) a.asVertex().getProperty(MyOtherBrain.KEY);
    }

    private Atom getAtom(final Vertex v) {
        return manager.frame(v, Atom.class);
    }

    private Atom getAtom(final String key) throws InvalidUpdateException {
        if (null == key) {
            throw new IllegalStateException("null atom key");
        }

        Vertex v;

        CloseableSequence<Vertex> s = keys.get(MyOtherBrain.KEY, key);
        try {
            if (!s.hasNext()) {
                throw new InvalidUpdateException("no such vertex: " + key);
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

    private Atom createAtom() {
        Vertex v = graph.addVertex(null);
        v.setProperty(MyOtherBrain.KEY, createKey());
        Atom a = manager.frame(v, Atom.class);
        a.setCreated(new Date().getTime());
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

    private Collection<Atom> getOutboundAssociations(final Atom from) {
        List<TimestampedAtom> c = new LinkedList<TimestampedAtom>();

        for (Edge e : from.asVertex().getInEdges()) {
            if (e.getLabel().equals(MyOtherBrain.FROM)) {
                c.add(new TimestampedAtom(getAtom(e.getOutVertex())));
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

    public static class NoSuchRootException extends Exception {
        public NoSuchRootException(final String message) {
            super(message);
        }
    }

    public static void main(final String[] args) throws Exception {
        NotesIO p = new NotesIO();
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
        NotesLens m = new NotesLens(graph, manager);
        Atom root = m.createAtom();
        root.asVertex().setProperty(MyOtherBrain.KEY, "00000");
        root.setText("Josh's notes");
        root.setType(".");
        m.update(root.getKey(), notes, 0);

        //GraphMLWriter.outputGraph(graph, System.out);
        //System.out.println();

        Note n = m.view((String) root.asVertex().getProperty(MyOtherBrain.KEY), 3);
        p.writeChildren(n, System.out);
    }
}
