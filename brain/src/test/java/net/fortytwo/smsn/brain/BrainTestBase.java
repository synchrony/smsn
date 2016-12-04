package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.pg.Neo4jGraphWrapper;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;
import net.fortytwo.smsn.brain.model.pg.TinkerGraphWrapper;
import net.fortytwo.smsn.brain.wiki.NoteReader;
import net.fortytwo.smsn.brain.wiki.NoteWriter;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;
import org.junit.After;
import org.junit.Before;

import java.io.File;
import java.io.IOException;
import java.util.Collection;

public abstract class BrainTestBase {

    protected NoteQueries queries;
    protected AtomGraph atomGraph;
    protected abstract AtomGraph createAtomGraph() throws IOException;

    protected final NoteReader parser = new NoteReader();
    protected final NoteWriter writer = new NoteWriter();

    protected Neo4jGraphWrapper graphWrapper;
    protected Filter filter = new Filter();
    protected Collection<Atom> result;

    protected AtomGraph createTinkerAtomGraph() {
        TinkerGraph g = TinkerGraph.open();
        TinkerGraphWrapper wrapper = new TinkerGraphWrapper(g);
        return new PGAtomGraph(wrapper);
    }

    protected AtomGraph createNeo4jAtomGraph() throws IOException {
        File dir = new File("/tmp/neo");
        dir.delete();

        graphWrapper = new Neo4jGraphWrapper(dir);

        return new PGAtomGraph(graphWrapper);
    }

    @Before
    public void setUp() throws Exception {
        atomGraph = createAtomGraph();
        Brain brain = new Brain(atomGraph);
        queries = new NoteQueries(brain);
        filter = new Filter();
    }

    @After
    public void tearDown() throws Exception {
        if (null != graphWrapper) {
            graphWrapper.shutdown();
        }
    }

    protected Atom importExample(final String exampleFile) throws IOException, NoteReader.NoteParsingException {
        Filter writeFilter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;

        Note rootNote = parser.fromWikiText(NoteReader.class.getResourceAsStream(exampleFile));
        rootNote.setId(SemanticSynchrony.createRandomId());
        Atom root = atomGraph.createAtom(filter, rootNote.getId());
        queries.update(rootNote, 5, writeFilter, style);
        return root;
    }

    protected int countAtoms(final AtomGraph atomGraph) {
        int count = 0;
        for (Atom a : atomGraph.getAllAtoms()) {
            count++;
        }
        return count;
    }
}
