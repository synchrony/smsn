package net.fortytwo.myotherbrain.notes;

import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.blueprints.pgm.impls.tg.TinkerGraph;
import com.tinkerpop.frames.FramesManager;
import junit.framework.TestCase;
import net.fortytwo.myotherbrain.MyOtherBrain;
import net.fortytwo.myotherbrain.model.frames.Atom;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * User: josh
 * Date: 6/20/11
 * Time: 7:58 PM
 */
public class NotesViewsTest extends TestCase {
    private IndexableGraph graph;
    private FramesManager manager;
    private NotesIO io;
    private NotesViews views;

    @Override
    public void setUp() throws Exception {
        graph = new TinkerGraph();
        manager = new FramesManager(graph);
        io = new NotesIO();
        views = new NotesViews(graph, manager);
    }

    @Override
    public void tearDown() throws Exception {
        graph.shutdown();
    }

    public void testAll() throws Exception {
        String s = "" +
                ".  a\n" +
                "    .  b\n" +
                "    .  c\n" +
                "V  d\n" +
                ".  e\n";

        graph.clear();
        Atom root = createAtom("1331");
        root.setType(".");
        root.setText("the root");

        List<Note> n = parse(s);
        views.toGraph(n, root);
        Atom r = getAtom("1331");
        assertEquals("the root", r.getText());
        assertEquals(".", r.getType());
        Set<Atom> as = getAssociatedAtoms(r);
        assertEquals(3, as.size());
        assertEquals(11, countVertices());
        assertEquals(10, countEdges());
        assertEquals("c", getAtom("6").getText());

        //Note o = views.toNote((String) root.element().getId(), null, 4);
        //io.writeChildren(o, System.out);
        //GraphMLWriter.outputGraph(graph, System.out);
        //System.out.println();

        /*
        String update = "" +
                "(00009:00000) .  a\n" +
                "(00001:00002)     .  b\n" +
                "(00005:00006)     .  c\n" +
                "(00017:00016) .  e\n" +
                "(00013:00012) V  d";
        List<Note> u = parse(update);
        //io.writeNotes(u, System.out);
        views.applyUpdate(u, (String) root.element().getId(), null, 4);
        //o = views.toNote((String) root.element().getId(), null, 4);
        //io.writeChildren(o, System.out);
        //System.out.println("############");
        assertEquals(11, countVertices());
        assertEquals(10, countEdges());
        assertEquals("c", getAtom("6").getText());

        update = "" +
                "(00009:00000) .  a\n" +
                "(00005:00006)     .  changed!\n" +
                "(00017:00016) .  e\n" +
                "    !  new child here\n" +
                "        .  new grandchild\n" +
                "(00013:00012) V  d";
        u = parse(update);
        //io.writeNotes(u, System.out);
        views.applyUpdate(u, (String) root.element().getId(), null, 4);
        //o = views.toNote((String) root.element().getId(), null, 4);
        //io.writeChildren(o, System.out);
        //System.out.println("############");
        assertEquals(14, countVertices());
        assertEquals(12, countEdges());
        assertEquals("changed!", getAtom("6").getText());
        */
    }

    private int countVertices() {
        int count = 0;
        for (Vertex v : graph.getVertices()) {
            count++;
        }
        return count;
    }

    private int countEdges() {
        int count = 0;
        for (Edge v : graph.getEdges()) {
            count++;
        }
        return count;
    }

    private Atom createAtom(final String id) {
        return manager.frame(graph.addVertex(id), Atom.class);
    }

    private Atom getAtom(final String id) {
        return manager.frame(graph.getVertex(id), Atom.class);
    }

    private List<Note> parse(final String s) throws IOException, NotesIO.NoteParsingException {
        InputStream in = new ByteArrayInputStream(s.getBytes());
        try {
            return io.parseNotes(in);
        } finally {
            in.close();
        }
    }

    private Set<Atom> getAssociatedAtoms(final Atom subject) {
        Set<Atom> s = new HashSet<Atom>();
        for (Atom ass : getAssociations(subject)) {
            for (Atom a : ass.getMembers()) {
                if (!a.element().getId().equals(subject.element().getId())) {
                    s.add(a);
                }
            }
        }

        return s;
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
}
