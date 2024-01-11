package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.json.JsonFormat;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.server.ActionContext;
import org.json.JSONObject;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

public class FindDuplicatesTest extends ActionTestBase {
    private Note note1, note2, note3;

    @Test
    public void noResultsIfNoDuplicates() throws Exception {
        note1 = createNoteWithTitle("one");
        note2 = createNoteWithTitle("two");

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        assertNull(view.optJSONObject(JsonFormat.Keys.CHILDREN));
    }

    @Test
    public void noResultsIfInexactMatch() throws Exception {
        note1 = createNoteWithTitle("earth");
        note2 = createNoteWithTitle("Earth");
        note3 = createNoteWithTitle("ear th");

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        assertNull(view.optJSONObject(JsonFormat.Keys.CHILDREN));
    }

    @Test
    public void resultsForExactMatch() throws Exception {
        topicGraph.begin();
        note1 = createNoteWithTitle("earth");
        note2 = createNoteWithTitle("earth");
        note3 = createNoteWithTitle("earth");
        topicGraph.commit();

        Note note = topicGraph.getNoteById(Note.getId(note1)).get();
        assertEquals("earth", Note.getTitle(note));
        assertEquals(3, countNotes());

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        assertEquals(3, view.optJSONArray(JsonFormat.Keys.CHILDREN).length());
        assertEquals("earth", view.optJSONArray(JsonFormat.Keys.CHILDREN).getJSONObject(0)
                .get(SemanticSynchrony.PropertyKeys.TITLE));
    }

    private FindDuplicates createAction() {
        FindDuplicates action = new FindDuplicates();
        action.setFilter(Filter.noFilter());
        return action;
    }
}
