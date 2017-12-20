package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.pg.PGNote;
import net.fortytwo.smsn.brain.model.pg.PGTopic;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

public class TopicsAndPagesTest extends BrainTestBase {

    private Note friendsNote, arthurNote, fordNote, zaphodNote, teaNote;

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createNeo4jTopicGraph();
    }

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();

        friendsNote = topicGraph.createNote(friendTopic, "friends", Role.Relation);
        arthurNote = topicGraph.createNote(arthurTopic, "Arthur P. Dent", null);
        fordNote = topicGraph.createNote(fordTopic, "Ford Prefect", null);
        zaphodNote = topicGraph.createNote(zaphodTopic, "Zaphod Beeblebrox", Role.Relation);
        teaNote = topicGraph.createNote(teaTopic, "Tea", null);
    }

    @Test
    public void ordinaryPageAndTopicTreeAreAllowed() {
        Note note = arthurNote;
        note.setText("a page about Arthur");
        note.setFirst(teaNote);
        assertEquals("a page about Arthur", note.getText());
        assertEquals("Arthur P. Dent", note.getLabel());

        Note firstChild = note.getFirst().get(0);
        assertEquals("Tea", firstChild.getLabel());
        assertEquals("tea", firstChild.getTopic().getId());

        fordNote.setRest(zaphodNote);
        friendsNote.setFirst(fordNote);
        note.add(1, friendsNote);

        assertEquals("friends", note.getFirst().get(1).getLabel());
        assertEquals("Ford Prefect",
                note.getFirst().get(1).getFirst().get(0).getLabel());

        note.destroy();
    }

    @Test(expected = IllegalArgumentException.class)
    public void linkWithoutTargetIsNotAllowed() {
        topicGraph.createNote(null, "nowhere", null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void linkWithoutLabelIsNotAllowed() {
        topicGraph.createNote(arthurTopic, null, null);
    }

    @Test(expected = NullPointerException.class)
    public void noteWithoutTopicIsNotAllowed() {
        topicGraph.createNote(null, "not allowed", Role.Entity);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void settingNoteTopicToNullIsNotAllowed() {
        arthurNote.setTopic(null);
    }

    @Test
    public void topicDAGIsDestroyedGracefully() {
        Note root = arthurNote;

        Object arthurId = ((PGTopic) arthurTopic).asVertex().id();
        assertTrue(graph.traversal().V(arthurId).hasNext());
        Object pageId = ((PGNote) root).asVertex().id();
        assertTrue(graph.traversal().V(pageId).hasNext());

        Note first = topicGraph.createNote(arthurTopic, "Arthur as a top-level link", null);
        first.setRest(topicGraph.createNote(arthurTopic, "Arthur as a header", Role.Relation));
        root.setFirst(first);
        root.getFirst().get(1).setFirst(
                topicGraph.createNote(arthurTopic, "Arthur as a second-level link", null));

        assertEquals(arthurTopic, root.getTopic());
        assertEquals("Arthur P. Dent", root.getLabel());
        assertNull(root.getRole());
        assertEquals(arthurTopic, root.getFirst().get(0).getTopic());
        assertEquals("Arthur as a top-level link", root.getFirst().get(0).getLabel());
        assertNull(root.getFirst().get(0).getRole());
        assertEquals(arthurTopic, root.getFirst().get(1).getTopic());
        assertEquals("Arthur as a header", root.getFirst().get(1).getLabel());
        assertEquals(Role.Relation, root.getFirst().get(1).getRole());
        assertEquals(arthurTopic, root.getFirst().get(1).getFirst().get(0).getTopic());
        assertEquals("Arthur as a second-level link",
                root.getFirst().get(1).getFirst().get(0).getLabel());
        assertNull(root.getFirst().get(1).getFirst().get(0).getRole());

        // before destroying the page, create another page about Arthur
        Note note2 = topicGraph.createNote(arthurTopic, "Arthur Philip Dent", null);
        Object page2Id = ((PGNote) note2).asVertex().id();

        assertTrue(graph.traversal().V(arthurId).hasNext());
        assertTrue(graph.traversal().V(pageId).hasNext());
        assertTrue(graph.traversal().V(page2Id).hasNext());

        root.destroy();

        // Arthur is still here, as it is also referenced by page2
        assertTrue(graph.traversal().V(arthurId).hasNext());
        assertFalse(graph.traversal().V(pageId).hasNext());
        assertTrue(graph.traversal().V(page2Id).hasNext());

        note2.destroy();

        assertFalse(graph.traversal().V(arthurId).hasNext());
        assertFalse(graph.traversal().V(pageId).hasNext());
        assertFalse(graph.traversal().V(page2Id).hasNext());
    }
}