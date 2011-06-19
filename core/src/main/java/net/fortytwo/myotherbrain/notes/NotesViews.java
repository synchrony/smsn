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
import java.util.LinkedList;
import java.util.List;

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

        if (null == atomId) {
            throw new IllegalStateException();
        }

        Atom av = getOrCreateAtom(atomId);
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

    public Atom toGraph(final NoteContext context) {
        Atom self = getOrCreateAtom(context.getAtomId());

        if (null != context.getText()) {
            self.setText(context.getText());
        }

        self.setType(".");

        for (NoteContext child : context.getChildren()) {
            Atom ass = getOrCreateAtom(child.getAssociationId());

            Atom c = toGraph(child);
            ass.addMember(self);
            ass.addMember(c);
        }

        for (Note child : context.getNotes()) {
            Atom ass = getOrCreateAtom(child.getAssociationId());

            Atom c = toGraph(child);
            ass.addMember(self);
            ass.addMember(c);
        }

        return self;
    }

    public Atom toGraph(final Note note) {

        Atom self = getOrCreateAtom(note.getAtomId());

        if (null != note.getText()) {
            self.setText(note.getText());
        }

        if (null != note.getType()) {
            self.setType(note.getType());
        }

        for (Note child : note.getChildren()) {
            Atom ass = getOrCreateAtom(child.getAssociationId());

            Atom c = toGraph(child);
            ass.addMember(self);
            ass.addMember(c);
        }

        return self;
    }

    public void toGraph(final List<NoteContext> notes,
                        final Atom ref) {
        for (NoteContext c : notes) {
            Atom a = toGraph(c);
            Atom ass = getOrCreateAtom(c.getAssociationId());
            ass.addMember(ref);
            ass.addMember(a);
        }
    }

    private Atom getOrCreateAtom(final String id) {
        Vertex v;
        boolean created = false;

        if (null == id) {
            v = graph.addVertex(null);
            created = true;
        } else {
            v = graph.getVertex(id);
            if (null == v) {
                throw new IllegalStateException();
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
                c.add(getOrCreateAtom((String) e.getOutVertex().getId()));
            }
        }

        return c;
    }

    public static void main(final String[] args) throws Exception {
        NotesIO p = new NotesIO();
        List<NoteContext> contexts;

        //InputStream in = new FileInputStream("/tmp/notes.txt");
        InputStream in = new FileInputStream("/Users/josh/notes/notes.txt");
        try {
            contexts = p.parse(in);
        } finally {
            in.close();
        }

        Graph graph = new TinkerGraph();
        FramesManager manager = new FramesManager(graph);
        NotesViews m = new NotesViews(graph, manager);
        Atom root = m.getOrCreateAtom(null);
        root.setText("Josh's notes");
        root.setType(".");

        m.toGraph(contexts, root);

        GraphMLWriter.outputGraph(graph, System.out);
        System.out.println();

        Note n = m.toNote((String) root.element().getId(), null, 3);
        p.writeChildren(n, System.out);
    }
}
