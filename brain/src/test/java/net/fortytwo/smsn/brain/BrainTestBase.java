package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.model.pg.GraphWrapper;
import net.fortytwo.smsn.brain.model.pg.Neo4jGraphWrapper;
import net.fortytwo.smsn.brain.model.pg.PGTopicGraph;
import net.fortytwo.smsn.brain.model.pg.TinkerGraphWrapper;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.brain.query.ViewStyle;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;
import org.junit.After;
import org.junit.Before;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.List;

import static org.junit.Assert.assertTrue;

public abstract class BrainTestBase {

    protected static final String ARTHUR_ID = "bxSoyLUM4w4RfitB";
    protected static final String FORD_ID = "QoTIPwLOID58u3Lr";
    protected static final String ZAPHOD_ID = "RqAUSvwi1H878V5j";

    protected interface DefaultSources {
        String
                PRIVATE = "private",
                PERSONAL = "personal",
                PUBLIC = "public",
                UNIVERSAL = "universal";
    }

    protected TreeViews queries;
    protected TopicGraph topicGraph;
    protected final WikiParser wikiParser = new WikiParser();

    protected Brain brain;
    protected Graph graph;
    protected GraphWrapper graphWrapper;
    protected Filter filter = Filter.noFilter();
    protected ViewStyle viewStyle = ViewStyle.Basic.Forward.getStyle();
    protected Collection<Atom> result;

    protected abstract TopicGraph createAtomGraph() throws IOException;

    protected TopicGraph createTinkerAtomGraph() {
        graph = TinkerGraph.open();
        graphWrapper = new TinkerGraphWrapper((TinkerGraph) graph);
        return new PGTopicGraph(graphWrapper);
    }

    protected TopicGraph createNeo4jAtomGraph() throws IOException {
        File dir = createTempDirectory();

        graphWrapper = new Neo4jGraphWrapper(dir);
        graphWrapper.begin();
        graph = graphWrapper.getGraph();

        return new PGTopicGraph(graphWrapper);
    }

    @Before
    public void setUp() throws Exception {
        topicGraph = createAtomGraph();
        brain = new Brain(topicGraph);
        queries = new TreeViews(brain);
        filter = Filter.noFilter();
    }

    @After
    public void tearDown() throws Exception {
        if (null != graphWrapper) {
            graphWrapper.shutdown();
        }
    }

    protected Note importNoteFromFile(final String exampleFile) throws IOException {
        return wikiParser.parse(Brain.class.getResourceAsStream(exampleFile));
    }

    protected Atom importAtomFromFile(final String exampleFile) throws IOException {
        Filter writeFilter = new Filter(0f, 0.5f, DefaultSources.PRIVATE, DefaultSources.PERSONAL);
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        Note rootNote = importNoteFromFile(exampleFile);
        rootNote.setId(SemanticSynchrony.createRandomId());
        Atom root = topicGraph.createAtomWithProperties(writeFilter, rootNote.getId());
        queries.update(rootNote, 5, writeFilter, style);
        return root;
    }

    protected Atom createAtom(final String title) {
        return createAtom(SemanticSynchrony.createRandomId(), title);
    }

    protected Atom createAtom(final String id, final String title) {
        Atom atom = topicGraph.createAtomWithProperties(filter, id);
        atom.setTitle(title);
        return atom;
    }

    protected int countAtoms(final TopicGraph graph) {
        int count = 0;
        for (Atom a : graph.getAllAtoms()) {
            count++;
        }
        return count;
    }

    protected int countAtoms() {
        return countAtoms(topicGraph);
    }

    protected static List<Atom> childList(final Atom atom) {
        return EntityList.toJavaList(atom.getChildren());
    }

    protected File createTempDirectory() throws IOException {
        File file = File.createTempFile("smsn-testing-", "");
        file.delete();

        file.mkdirs();
        file.deleteOnExit();
        assertTrue(file.exists());
        assertTrue(file.isDirectory());
        return file;
    }
}
