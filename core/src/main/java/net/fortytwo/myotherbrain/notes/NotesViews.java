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
public class NotesViews {
    private static final String KEYS = "keys";

    private final IndexableGraph graph;
    private final FramesManager manager;
    private final Index<Vertex> keys;

    public NotesViews(final IndexableGraph graph,
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

    private String getKey(final Atom a) {
        return (String) a.element().getProperty(MyOtherBrain.KEY);
    }

    public Note toNote(final String atomKey,
                       final Atom parent,
                       final int levels) {
        //System.out.println("toNote(" + atomKey + ", " + parent + ", " + levels + ")");
        String parentKey = null == parent ? null : getKey(parent);

        Atom av = getAtom(atomKey);
        String type = av.getType();
        String text = av.getText();
        Note n = new Note(type, text);
        n.setAtomKey(atomKey);

        if (levels > 1) {
            for (Atom ass : getAssociations(av)) {
                Collection<Atom> members = ass.getMembers();
                if (2 != members.size()) {
                    throw new IllegalArgumentException("association of order " + members.size() + " is not allowed");
                }

                for (Atom m : members) {
                    if (!getKey(m).equals(atomKey)
                            && (null == parentKey || !getKey(m).equals(parentKey))) {
                        Note n2 = toNote(getKey(m), av, levels - 1);
                        n2.setAssociationKey(getKey(ass));

                        n.addChild(n2);
                    }
                }
            }
        }

        //System.out.println("yielding note: " + n);
        return n;
    }

    public Atom toGraph(final Note note,
                        final boolean recursive) {

        Atom self = getOrCreateAtom(note.getAtomKey());

        if (null != note.getText()) {
            self.setText(note.getText());
        }

        if (null != note.getType()) {
            self.setType(note.getType());
        }

        if (recursive) {
            for (Note child : note.getChildren()) {
                Atom ass = getOrCreateAtom(child.getAssociationKey());

                Atom c = toGraph(child, true);
                if (0 == ass.getMembers().size()) {
                    ass.addMember(self);
                    ass.addMember(c);
                }
            }
        }

        return self;
    }

    public void toGraph(final List<Note> notes,
                        final Atom ref) {
        for (Note c : notes) {
            Atom a = toGraph(c, true);
            Atom ass = getOrCreateAtom(c.getAssociationKey());
            if (0 == ass.getMembers().size()) {
                ass.addMember(ref);
                ass.addMember(a);
            }
        }
    }

    public void applyUpdate(final List<Note> update,
                            final String root,
                            final String parent,
                            final int levels) throws UpdateException {
        applyUpdate(update, getAtom(root), null == parent ? null : getAtom(parent), levels);
    }

    private void applyUpdate(final List<Note> update,
                             final Atom root,
                             final Atom parent,
                             final int levels) throws UpdateException {
        if (1 > levels) {
            return;
        }

        List<Note> before = toNote((String) getKey(root), parent, 2).getChildren();

        Map<String, Note> beforeMap = new HashMap<String, Note>();
        for (Note n : before) {
            //System.out.println("\tbefore: " + n.getAssociationId() + ", " + n.getAtomId());
            beforeMap.put(n.getAssociationKey(), n);
        }

        Map<String, Note> afterMap = new HashMap<String, Note>();
        for (Note n : update) {
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
                    throw new UpdateException("non-null association ID with null atom ID");
                } else if (!a.getAtomKey().equals(b.getAtomKey())) {
                    throw new UpdateException("atom ID of updated association has changed");
                }
            } else {
                //System.out.println("breaking association " + assId);
                breakAssociation(assId);
            }
        }

        // Add any new associations, and update fields
        for (Note n : update) {
            String assId = n.getAssociationKey();
            //System.out.println("assId = " + assId);
            Atom a;

            if (null == assId || null == beforeMap.get(assId)) {
                a = toGraph(n, false);

                Atom ass = getOrCreateAtom(null);
                ass.addMember(root);
                ass.addMember(a);
            } else {
                if (null == n.getAtomKey()) {
                    throw new UpdateException("non-null association ID with null atom ID");
                } else if (!n.getAtomKey().equals(beforeMap.get(assId).getAtomKey())) {
                    throw new UpdateException("atom ID of updated association has changed");
                }

                a = toGraph(n, false);
            }

            applyUpdate(n.getChildren(), a, root, levels - 1);
        }
    }

    private void breakAssociation(final String assKey) {
        graph.removeVertex(getAtom(assKey).element());
    }

    public static class UpdateException extends Exception {
        public UpdateException(final String message) {
            super(message);
        }
    }

    private Atom getAtom(final String key) {
        if (null == key) {
            throw new IllegalStateException();
        }

        Vertex v;

        CloseableSequence<Vertex> s = keys.get(MyOtherBrain.KEY, key);
        try {
            if (!s.hasNext()) {
                throw new IllegalStateException("no such vertex: " + key);
            }
            v = s.next();
            if (s.hasNext()) {
                throw new IllegalStateException("multiple vertices with the same key: '" + key + "'");
            }
        } finally {
            s.close();
        }

        return manager.frame(v, Atom.class);
    }

    private Atom getOrCreateAtom(final String key) {
        if (null == key) {
            Vertex v = graph.addVertex(null);
            v.setProperty(MyOtherBrain.KEY, createKey());
            Atom a = manager.frame(v, Atom.class);
            a.setCreated(new Date().getTime());
            return a;
        } else {
            return getAtom(key);
        }
    }

    private static final Random RANDOM = new Random();

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

    private Collection<Atom> getAssociations(final Atom subject) {
        Collection<Atom> c = new LinkedList<Atom>();

        for (Edge e : subject.element().getInEdges()) {
            if (e.getLabel().equals(MyOtherBrain.MEMBER)) {
                c.add(getAtom((String) e.getOutVertex().getProperty(MyOtherBrain.KEY)));
            }
        }

        return c;
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
        NotesViews m = new NotesViews(graph, manager);
        Atom root = m.getOrCreateAtom(null);
        root.element().setProperty(MyOtherBrain.KEY, "root");
        root.setText("Josh's notes");
        root.setType(".");
        m.toGraph(notes, root);

        //GraphMLWriter.outputGraph(graph, System.out);
        //System.out.println();

        Note n = m.toNote((String) root.element().getProperty(MyOtherBrain.KEY), null, 3);
        p.writeChildren(n, System.out);
    }
}
