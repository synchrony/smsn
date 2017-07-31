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
        Note.setText(note, "the page");
        assertEquals(1, countNotes());
        topicGraph.commit();

        note = topicGraph.getNoteById(Note.getId(note)).get();
        assertEquals("before", Note.getTitle(note));
        assertEquals("the page", Note.getText(note));

        assertEquals(1, countNotes());
    }

    @Test
    public void titleIsSetCorrectly() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.TITLE);
        action.setValue("after");

        perform(action);

        note = topicGraph.getNoteById(Note.getId(note)).get();
        assertEquals("after", Note.getTitle(note));
    }

    @Test(expected = RequestProcessingException.class)
    public void emptyTitleIsError() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.TITLE);
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

        note = topicGraph.getNoteById(Note.getId(note)).get();
        assertEquals("after", Note.getText(note));
    }

    @Test
    public void emptyPageBecomesNullPage() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.TEXT);
        action.setValue("  \n ");

        perform(action);

        note = topicGraph.getNoteById(Note.getId(note)).get();
        assertNull(Note.getText(note));
    }

    @Test
    public void weightIsSetCorrectly() throws Exception {
        topicGraph.begin();
        Note note = createNoteWithTitle("test");
        Note.setWeight(note, 0.25f);
        topicGraph.commit();

        note = topicGraph.getNoteById(Note.getId(note)).get();
        assertEquals(0.25f, Note.getWeight(note), 0.0f);

        SetProperties action = createAction();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.WEIGHT);
        action.setValue(0.5);

        perform(action);

        note = topicGraph.getNoteById(Note.getId(note)).get();
        assertEquals(0.5f, Note.getWeight(note), 0.0f);
    }

    @Test
    public void sourceIsSetCorrectly() throws Exception {
        topicGraph.begin();
        Note note = createNoteWithTitle("test");
        Note.setSource(note, DefaultSources.PRIVATE);
        topicGraph.commit();

        note = topicGraph.getNoteById(Note.getId(note)).get();
        assertEquals(DefaultSources.PRIVATE, Note.getSource(note));

        SetProperties action = createAction();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.SOURCE);
        action.setValue(DefaultSources.PERSONAL);

        perform(action);

        note = topicGraph.getNoteById(Note.getId(note)).get();
        assertEquals(DefaultSources.PERSONAL, Note.getSource(note));
    }

    @Test
    public void priorityIsSetCorrectly() throws Exception {
        topicGraph.begin();
        Note note = createNoteWithTitle("test");
        Note.setPriority(note, 0.25f);
        topicGraph.commit();

        note = topicGraph.getNoteById(Note.getId(note)).get();
        assertEquals(0.25f, Note.getPriority(note), 0.0f);

        SetProperties action = createAction();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.PRIORITY);
        action.setValue(0.5);

        perform(action);

        note = topicGraph.getNoteById(Note.getId(note)).get();
        assertEquals(0.5f, Note.getPriority(note), 0.0f);
    }

    @Test
    public void shortcutIsSetCorrectly() throws Exception {
        topicGraph.begin();
        Note note = createNoteWithTitle("test");
        topicGraph.commit();

        note = topicGraph.getNoteById(Note.getId(note)).get();
        assertNull(Note.getShortcut(note));

        SetProperties action = createAction();
        action.setId(Note.getId(note));
        action.setName(SemanticSynchrony.PropertyKeys.SHORTCUT);
        action.setValue("after");

        perform(action);

        note = topicGraph.getNoteById(Note.getId(note)).get();
        assertEquals("after", Note.getShortcut(note));
    }

    private SetProperties createAction() {
        SetProperties action = new SetProperties();
        action.setFilter(Filter.noFilter());
        return action;
    }
}
