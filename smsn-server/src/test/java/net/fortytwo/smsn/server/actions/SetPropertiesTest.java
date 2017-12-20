package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class SetPropertiesTest extends ActionTestBase {
    private Note note;

    @Before
    public void setUp() throws Exception {
        super.setUp();

        topicGraph.begin();
        note = createNoteWithTitle("before");
        note.setText("the page");
        assertEquals(1, countNotes());
        topicGraph.commit();

        note = topicGraph.getTopicById(Note.getId(note)).get();
        assertEquals("before", note.getLabel());
        assertEquals("the page", note.getText());

        assertEquals(1, countNotes());
    }

    @Test
    public void titleIsSetCorrectly() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.LABEL);
        action.setValue("after");

        perform(action);

        note = topicGraph.getTopicById(Note.getId(note)).get();
        assertEquals("after", note.getLabel());
    }

    @Test(expected = RequestProcessingException.class)
    public void emptyTitleIsError() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.LABEL);
        action.setValue("  \n");

        perform(action);
    }

    @Test
    public void pageIsSetCorrectly() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.TEXT);
        action.setValue("after");

        perform(action);

        note = topicGraph.getTopicById(Note.getId(note)).get();
        assertEquals("after", note.getText());
    }

    @Test
    public void emptyPageBecomesNullPage() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.TEXT);
        action.setValue("  \n ");

        perform(action);

        note = topicGraph.getTopicById(Note.getId(note)).get();
        assertNull(note.getText());
    }

    @Test
    public void weightIsSetCorrectly() throws Exception {
        topicGraph.begin();
        Note note = createNoteWithTitle("test");
        note.setWeight(0.25f);
        topicGraph.commit();

        note = topicGraph.getTopicById(Note.getId(note)).get();
        assertEquals(0.25f, note.getWeight(), 0.0f);

        SetProperties action = createAction();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.WEIGHT);
        action.setValue(0.5);

        perform(action);

        note = topicGraph.getTopicById(Note.getId(note)).get();
        assertEquals(0.5f, note.getWeight(), 0.0f);
    }

    @Test
    public void sourceIsSetCorrectly() throws Exception {
        topicGraph.begin();
        Note note = createNoteWithTitle("test");
        note.setSource(DefaultSources.PRIVATE);
        topicGraph.commit();

        note = topicGraph.getTopicById(Note.getId(note)).get();
        assertEquals(DefaultSources.PRIVATE, note.getSource());

        SetProperties action = createAction();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.SOURCE);
        action.setValue(DefaultSources.PERSONAL);

        perform(action);

        note = topicGraph.getTopicById(Note.getId(note)).get();
        assertEquals(DefaultSources.PERSONAL, note.getSource());
    }

    @Test
    public void priorityIsSetCorrectly() throws Exception {
        topicGraph.begin();
        Note note = createNoteWithTitle("test");
        note.setPriority(0.25f);
        topicGraph.commit();

        note = topicGraph.getTopicById(Note.getId(note)).get();
        assertEquals(0.25f, note.getPriority(), 0.0f);

        SetProperties action = createAction();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.PRIORITY);
        action.setValue(0.5);

        perform(action);

        note = topicGraph.getTopicById(Note.getId(note)).get();
        assertEquals(0.5f, note.getPriority(), 0.0f);
    }

    @Test
    public void shortcutIsSetCorrectly() throws Exception {
        topicGraph.begin();
        Note note = createNoteWithTitle("test");
        topicGraph.commit();

        note = topicGraph.getTopicById(Note.getId(note)).get();
        assertNull(note.getShortcut());

        SetProperties action = createAction();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.SHORTCUT);
        action.setValue("after");

        perform(action);

        note = topicGraph.getTopicById(Note.getId(note)).get();
        assertEquals("after", note.getShortcut());
    }

    private SetProperties createAction() {
        SetProperties action = new SetProperties();
        action.setFilter(Filter.noFilter());
        return action;
    }
}
