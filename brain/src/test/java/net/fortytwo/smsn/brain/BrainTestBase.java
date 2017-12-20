package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.model.pg.GraphWrapper;
import net.fortytwo.smsn.brain.model.pg.PGTopicGraph;
import net.fortytwo.smsn.brain.model.pg.neo4j.Neo4jGraphWrapper;
import net.fortytwo.smsn.brain.model.pg.tg.TinkerGraphWrapper;
import net.fortytwo.smsn.brain.query.Model;
import net.fortytwo.smsn.brain.query.ViewStyle;
import net.fortytwo.smsn.config.DataSource;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;
import org.junit.After;
import org.junit.Before;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public abstract class BrainTestBase {

    protected static final String ARTHUR_ID = "bxSoyLUM4w4RfitB";
    protected static final String FORD_ID = "QoTIPwLOID58u3Lr";
    protected static final String ZAPHOD_ID = "RqAUSvwi1H878V5j";

    protected Topic arthurTopic, fordTopic, zaphodTopic, friendTopic, earthTopic, teaTopic;

    protected final String parserTopicId = ARTHUR_ID;
    protected final String parserLabel = "Arthur Dent";
    protected final String parserSource = "universal";

    protected interface DefaultSources {
        String
                PRIVATE = "private",
                PERSONAL = "personal",
                PUBLIC = "public",
                UNIVERSAL = "universal";
    }

    protected Model model;
    protected TopicGraph topicGraph;
    protected final WikiParser wikiParser = new WikiParser();

    protected Brain brain;
    protected Graph graph;
    protected GraphWrapper graphWrapper;
    protected Filter filter = Filter.noFilter();
    protected final ViewStyle viewStyle = ViewStyle.Basic.Forward.getStyle();
    protected Iterable<Note> queryResult;

    @Before
    public void setUp() throws Exception {
        topicGraph = createTopicGraph();
        brain = new Brain(topicGraph);
        model = new Model(brain);
        filter = Filter.noFilter();

        arthurTopic = createTopic(ARTHUR_ID);
        fordTopic = createTopic(FORD_ID);
        zaphodTopic = createTopic(ZAPHOD_ID);
        friendTopic = createTopic("friend");
        earthTopic = createTopic("earth");
        teaTopic = createTopic("tea");
    }

    @After
    public void tearDown() throws Exception {
        if (null != graphWrapper) {
            graphWrapper.shutdown();
        }
    }

    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }

    protected TopicGraph createTinkerTopicGraph() {
        graph = TinkerGraph.open();
        graphWrapper = new TinkerGraphWrapper((TinkerGraph) graph);
        return new PGTopicGraph(graphWrapper);
    }

    protected TopicGraph createNeo4jTopicGraph() throws IOException {
        File dir = createTempDirectory();

        graphWrapper = new Neo4jGraphWrapper(dir);
        graphWrapper.begin();
        graph = graphWrapper.getGraph();

        return new PGTopicGraph(graphWrapper);
    }

    protected Note importNodeFromFile(final String exampleFile) throws IOException {
        return parseToNote(Brain.class.getResourceAsStream(exampleFile));
    }

    protected Note importNoteFromFile(final String exampleFile) throws IOException {
        Filter writeFilter = new Filter(0f, 0.5f, DefaultSources.PRIVATE, DefaultSources.PERSONAL);
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        Note rootNode = importNodeFromFile(exampleFile);
        Model.setTopicId(rootNode, SemanticSynchrony.createRandomId());
        Note root = topicGraph.createNoteWithProperties(writeFilter, rootNode.getTopic());
        model.view().root(root).height(5).filter(writeFilter).style(style).put(rootNode);
        return root;
    }

    protected Note createNote(final String id) {
        Topic topic = topicGraph.createTopic(id);
        return topicGraph.createNoteWithProperties(filter, topic);
    }

    protected Note createNote() {
        return createNote(null);
    }

    protected Note createNoteWithTitle(final String title) {
        return createNote(null, title);
    }

    protected Note createNote(final String topicId, final String label) {
        Topic topic = topicGraph.createTopic(topicId);
        Note note = topicGraph.createNoteWithProperties(filter, topic);
        note.setLabel(label);
        return note;
    }

    protected Topic createTopic(final String id) {
        return topicGraph.createTopic(id);
    }

    protected Topic createTopicDTO(final String id) {
        Topic topic = new TopicDTO();
        topic.setId(id);
        return topic;
    }

    protected Note createNoteDTO(final String topicId, final String label) {
        Topic topic = createTopicDTO(topicId);
        Note note = new NoteDTO();
        note.setTopic(topic);
        note.setLabel(label);
        return note;
    }

    protected int countNotes(final TopicGraph graph) {
        int count = 0;
        for (Note ignored : graph.getAllNotes()) {
            count++;
        }
        return count;
    }

    protected int countNotes() {
        return countNotes(topicGraph);
    }

    protected static List<Note> childList(final Note note) {
        return ListNode.toJavaList(note.getFirst());
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

    protected Note parseToNote(final String input) throws IOException {
        return wikiParser.parse(input);
    }

    protected Note parseToNote(final InputStream input) throws IOException {
        return wikiParser.parse(input);
    }

    protected void assertNodesEqual(final Note a,
                                    final String... expected) {
        String[] actual = new String[(int) countChildren(a)];

        int i = 0;
        ListNode<Note> cur = a.getFirst();
        while (null != cur) {
            actual[i++] = cur.getFirst().getLabel();
            cur = cur.getRest();
        }

        assertArrayEquals(expected, actual);
    }

    protected int count(final Iterable iter) {
        return count(iter.iterator());
    }

    protected int count(final Iterator iter) {
        int count = 0;
        while (iter.hasNext()) {
            iter.next();
            count++;
        }
        return count;
    }

    protected long countChildren(final Note a) {
        ListNode<Note> children = a.getFirst();
        return null == children ? 0 : ListNode.toJavaList(children).size();
    }

    protected void assertChildCount(final int expected, final Note note) {
        Note children = note.getFirst();
        int actual = null == children ? 0 : children.length();
        assertEquals(expected, actual);
    }

    protected Note getNote(final String topicId) {
        Optional<Topic> opt = topicGraph.getTopicById(topicId);
        return opt.map(this::getNote).orElse(null);
    }

    protected Note getNote(final Topic topic) {
        Iterator<Note> iter = topic.getNotes();
        if (iter.hasNext()) {
            Note result = iter.next();
            assertFalse("more than one note for topic " + topic, iter.hasNext());
            return result;
        } else {
            return null;
        }
    }

    protected File createVCSTestDirectory() throws IOException {
        File dir = createTempDirectory();

        File privateDir = new File(dir, "private");
        privateDir.mkdir();
        File personalDir = new File(dir, "personal");
        personalDir.mkdir();
        File publicDir = new File(dir, "public");
        publicDir.mkdir();
        File universalDir = new File(dir, "universal");
        universalDir.mkdir();

        List<DataSource> sources = SemanticSynchrony.getConfiguration().getSources();
        sources.get(0).setLocation(privateDir.getAbsolutePath());
        sources.get(1).setLocation(personalDir.getAbsolutePath());
        sources.get(2).setLocation(publicDir.getAbsolutePath());
        sources.get(3).setLocation(universalDir.getAbsolutePath());

        return dir;
    }
}
