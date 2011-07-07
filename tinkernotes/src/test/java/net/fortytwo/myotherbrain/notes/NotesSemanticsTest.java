package net.fortytwo.myotherbrain.notes;

import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.blueprints.pgm.impls.tg.TinkerGraph;
import com.tinkerpop.frames.FramesManager;
import junit.framework.TestCase;
import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.MyOtherBrain;
import org.json.JSONObject;

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
public class NotesSemanticsTest extends TestCase {
    private IndexableGraph graph;
    private FramesManager manager;
    private NotesSyntax syntax;
    private NotesSemantics semantics;

    @Override
    public void setUp() throws Exception {
        graph = new TinkerGraph();
        manager = new FramesManager(graph);
        syntax = new NotesSyntax();
        semantics = new NotesSemantics(graph, manager);
    }

    @Override
    public void tearDown() throws Exception {
        graph.shutdown();
    }

    public void testEncoding() throws Exception {
        Filter f = new Filter();

        Atom root = createAtom("11111");

        Note rootNote = new Note("foo");
        Note child = new Note("cheval ˆ phynances");
        child.setLinkValue("a link");
        rootNote.addChild(child);

        //System.out.println(before.getTargetValue());

        semantics.update(root, rootNote.getChildren(), 1, f, NotesSemantics.ViewStyle.TARGETS);

        Note after = semantics.view(root, 1, f, NotesSemantics.ViewStyle.TARGETS);

        JSONObject json = syntax.toJSON(after);
        //System.out.println(json.toString());
        JSONObject j = json.getJSONArray("children").getJSONObject(0);
        assertEquals("cheval ˆ phynances", j.getJSONObject("target").getString("value"));
    }

    public void testAll() throws Exception {
        /*
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

    private Atom createAtom(final String key) {
        Atom a = manager.frame(graph.addVertex(null), Atom.class);
        a.setKey(key);
        a.setWeight(0.5f);
        a.setSharability(0.5f);
        return a;
    }

    private Atom getAtom(final String id) {
        return manager.frame(graph.getVertex(id), Atom.class);
    }

    private Atom getAtom(final Vertex v) {
        return manager.frame(v, Atom.class);
    }

    private List<Note> parse(final String s) throws IOException, NotesSyntax.NoteParsingException {
        InputStream in = new ByteArrayInputStream(s.getBytes());
        try {
            return syntax.readNotes(in);
        } finally {
            in.close();
        }
    }

    private Set<Atom> getAssociatedAtoms(final Atom subject) {
        Set<Atom> s = new HashSet<Atom>();
        for (Atom ass : getOutboundAssociations(subject)) {

        }

        return s;
    }

    private Collection<Atom> getOutboundAssociations(final Atom subject) {
        Collection<Atom> c = new LinkedList<Atom>();

        for (Edge e : subject.asVertex().getInEdges()) {
            if (e.getLabel().equals(MyOtherBrain.FROM)) {
                c.add(getAtom(e.getOutVertex()));
            }
        }

        return c;
    }
}
