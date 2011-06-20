package net.fortytwo.myotherbrain.notes;

import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.blueprints.pgm.impls.tg.TinkerGraph;
import com.tinkerpop.blueprints.pgm.util.graphml.GraphMLWriter;
import com.tinkerpop.frames.FramesManager;
import net.fortytwo.myotherbrain.MyOtherBrain;
import net.fortytwo.myotherbrain.model.frames.Atom;

import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * User: josh
 * Date: 6/18/11
 * Time: 7:26 PM
 */
public class NotesViews {
    private final Graph graph;
    private final FramesManager manager;

    public NotesViews(Graph graph, FramesManager manager) {
        this.graph = graph;
        this.manager = manager;
    }

    public Note toNote(final String atomId,
                       final Atom parent,
                       final int levels) {
        String parentId = null == parent ? null : (String) parent.element().getId();

        Atom av = getAtom(atomId);
        String type = av.getType();
        String text = av.getText();
        Note n = new Note(type, text);
        n.setAtomId(atomId);

        if (levels > 1) {
            for (Atom ass : getAssociations(av)) {
                Collection<Atom> members = ass.getMembers();
                if (2 != members.size()) {
                    throw new IllegalArgumentException("association of order " + members.size() + " is not allowed");
                }

                for (Atom m : members) {
                    if (!m.element().getId().equals(atomId)
                            && (null == parentId || !m.element().getId().equals(parentId))) {
                        Note n2 = toNote((String) m.element().getId(), av, levels - 1);
                        n2.setAssociationId((String) ass.element().getId());

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

        Atom self = getOrCreateAtom(note.getAtomId());

        if (null != note.getText()) {
            self.setText(note.getText());
        }

        if (null != note.getType()) {
            self.setType(note.getType());
        }

        if (recursive) {
            for (Note child : note.getChildren()) {
                Atom ass = getOrCreateAtom(child.getAssociationId());

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
            Atom ass = getOrCreateAtom(c.getAssociationId());
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

    private String normalizeId(final String id) {
        if (null == id) {
            return null;
        }

        return new Integer(id).toString();
    }

    private void normalizeIds(final List<Note> notes) {
        for (Note n : notes) {
            n.setAtomId(normalizeId(n.getAtomId()));
            n.setAssociationId(normalizeId(n.getAssociationId()));
        }
    }

    private void applyUpdate(final List<Note> update,
                             final Atom root,
                             final Atom parent,
                             final int levels) throws UpdateException {
        //System.out.println("applying update at root: " + root.element().getId());

        if (1 > levels) {
            return;
        }

        List<Note> before = toNote((String) root.element().getId(), parent, 2).getChildren();

        // FIXME: this is a hack
        normalizeIds(update);
        normalizeIds(before);

        Map<String, Note> beforeMap = new HashMap<String, Note>();
        for (Note n : before) {
            //System.out.println("\tbefore: " + n.getAssociationId() + ", " + n.getAtomId());
            beforeMap.put(n.getAssociationId(), n);
        }

        Map<String, Note> afterMap = new HashMap<String, Note>();
        for (Note n : update) {
            if (null != n.getAssociationId()) {
                //System.out.println("\tafter: " + n.getAssociationId() + ", " + n.getAtomId());
                afterMap.put(n.getAssociationId(), n);
            }
        }

        // Remove any deleted associations
        for (String assId : beforeMap.keySet()) {
            /*boolean keep = true;
            for (Atom m : getAtom(assId).getMembers()) {
                String id = (String) m.element().getId();
                if (id.equals(root))
            }
              */
            if (afterMap.keySet().contains(assId)) {
                Note b = beforeMap.get(assId);
                Note a = afterMap.get(assId);
                if (null == a.getAtomId()) {
                    throw new UpdateException("non-null association ID with null atom ID");
                } else if (!a.getAtomId().equals(b.getAtomId())) {
                    throw new UpdateException("atom ID of updated association has changed");
                }
            } else {
                //System.out.println("breaking association " + assId);
                breakAssociation(assId);
            }
        }

        // Add any new associations, and update fields
        for (Note n : update) {
            String assId = n.getAssociationId();
            //System.out.println("assId = " + assId);
            Atom a;

            if (null == assId || null == beforeMap.get(assId)) {
                a = toGraph(n, false);

                Atom ass = getOrCreateAtom(null);
                ass.addMember(root);
                ass.addMember(a);
            } else {
                if (null == n.getAtomId()) {
                    throw new UpdateException("non-null association ID with null atom ID");
                } else if (!n.getAtomId().equals(beforeMap.get(assId).getAtomId())) {
                    throw new UpdateException("atom ID of updated association has changed");
                }

                a = toGraph(n, false);
            }

            applyUpdate(n.getChildren(), a, root, levels - 1);
        }
    }

    private void breakAssociation(final String assId) {
        //Atom a = manager.frame(graph.getVertex(assId), Atom.class);
        graph.removeVertex(graph.getVertex(assId));
    }

    public static class UpdateException extends Exception {
        public UpdateException(final String message) {
            super(message);
        }
    }

    private Atom getAtom(final String id) {
        if (null == id) {
            throw new IllegalStateException();
        }

        return manager.frame(graph.getVertex(id), Atom.class);
    }

    private Atom getOrCreateAtom(final String id) {
        Vertex v;
        boolean created = false;

        if (null == id) {
            v = graph.addVertex(null);
            //System.out.println("created vertex: " + v.getId());
            //if ("29".equals(v.getId())) {
            //    new Exception().printStackTrace();
            //}
            created = true;
        } else {
            v = graph.getVertex(id);
            if (null == v) {
                throw new IllegalStateException("no such vertex: " + id);
            }
        }

        Atom a = manager.frame(v, Atom.class);
        if (created) {
            a.setCreated(new Date().getTime());
        }
        return a;
    }

    private Collection<Atom> getAssociations(final Atom subject) {
        Collection<Atom> c = new LinkedList<Atom>();

        for (Edge e : subject.element().getInEdges()) {
            if (e.getLabel().equals(MyOtherBrain.MEMBER)) {
                c.add(getAtom((String) e.getOutVertex().getId()));
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

        Graph graph = new TinkerGraph();
        FramesManager manager = new FramesManager(graph);
        NotesViews m = new NotesViews(graph, manager);
        Atom root = m.getOrCreateAtom(null);
        root.setText("Josh's notes");
        root.setType(".");

        m.toGraph(notes, root);

        GraphMLWriter.outputGraph(graph, System.out);
        System.out.println();

        Note n = m.toNote((String) root.element().getId(), null, 3);
        p.writeChildren(n, System.out);
    }
}
