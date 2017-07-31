package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.dto.LinkDTO;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.model.pg.GraphWrapper;
import net.fortytwo.smsn.brain.model.pg.PGTopicGraph;
import net.fortytwo.smsn.brain.model.pg.neo4j.Neo4jGraphWrapper;
import net.fortytwo.smsn.brain.model.pg.tg.TinkerGraphWrapper;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.brain.query.ViewStyle;
import net.fortytwo.smsn.config.DataSource;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;
import org.junit.After;
import org.junit.Before;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.List;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertArrayEquals;
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

    protected TreeViews queries;
    protected TopicGraph topicGraph;
    protected final WikiParser wikiParser = new WikiParser();

    protected Brain brain;
    protected Graph graph;
    protected GraphWrapper graphWrapper;
    protected Filter filter = Filter.noFilter();
    protected final ViewStyle viewStyle = ViewStyle.Basic.Forward.getStyle();
    protected Collection<Note> queryResult;

    @Before
    public void setUp() throws Exception {
        topicGraph = createTopicGraph();
        brain = new Brain(topicGraph);
        queries = new TreeViews(brain);
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

    protected TreeNode<Link> importNodeFromFile(final String exampleFile) throws IOException {
        return parseToTree(Brain.class.getResourceAsStream(exampleFile));
    }

    protected Note importNoteFromFile(final String exampleFile) throws IOException {
        Filter writeFilter = new Filter(0f, 0.5f, DefaultSources.PRIVATE, DefaultSources.PERSONAL);
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        TreeNode<Link> rootNode = importNodeFromFile(exampleFile);
        TreeViews.setId(rootNode, SemanticSynchrony.createRandomId());
        Note root = topicGraph.createNoteWithProperties(writeFilter, TreeViews.getId(rootNode));
        queries.update(rootNode, 5, writeFilter, style);
        return root;
    }

    protected Note createNote(final String id) {
        return topicGraph.createNoteWithProperties(filter, id );
    }

    protected Note createNote() {
        return createNote(null);
    }

    protected Note createNoteWithTitle(final String title) {
        return createNote(null, title);
    }

    protected Note createNote(final String id, final String title) {
        Note note = topicGraph.createNoteWithProperties(filter, id);
        Note.setTitle(note, title);
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

    protected TreeNode<Link> createTreeDTO(final String topicId, final String label) {
        Topic topic = createTopicDTO(topicId);
        Link link = new LinkDTO();
        link.setTarget(topic);
        link.setLabel(label);
        TreeNode<Link> tree = new TreeNodeDTO<>();
        tree.setValue(link);
        return tree;
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
        return ListNode.toJavaList(note.getChildren());
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


    protected TreeNode<Link> parseToTree(final String s) throws IOException {
        return parseToPage(s).getContent();
    }

    protected TreeNode<Link> parseToTree(final InputStream in) throws IOException {
        return parseToPage(in).getContent();
    }

    protected void setContent(final Page page) {
        page.setSource(parserSource);
        page.getContent().getValue().setLabel(parserLabel);
        page.getContent().getValue().getTarget().setId(parserTopicId);
    }

    protected Page parseToPage(final String input) throws IOException {
        Page page = wikiParser.parse(input);
        setContent(page);
        return page;
    }

    protected Page parseToPage(final InputStream input) throws IOException {
        Page page = wikiParser.parse(input);
        setContent(page);
        return page;
    }

    protected void assertNodesEqual(final Note a,
                                    final String... expected) {
        String[] actual = new String[(int) countChildren(a)];

        int i = 0;
        ListNode<Note> cur = a.getChildren();
        while (null != cur) {
            actual[i++] = Note.getTitle(cur.getFirst());
            cur = cur.getRest();
        }

        assertArrayEquals(expected, actual);
    }

    protected long countChildren(final Note a) {
        ListNode<Note> children = a.getChildren();
        return null == children ? 0 : ListNode.toJavaList(children).size();
    }

    protected void assertChildCount(final int expected, final TreeNode<Link> node) {
        ListNode<TreeNode<Link>> children = node.getChildren();
        int actual = null == children ? 0 : children.length();
        assertEquals(expected, actual);
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
