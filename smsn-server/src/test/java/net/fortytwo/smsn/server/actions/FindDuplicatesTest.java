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
    private Note atom1, atom2, atom3;

    @Test
    public void noResultsIfNoDuplicates() throws Exception {
        atom1 = createAtomWithTitle("one");
        atom2 = createAtomWithTitle("two");

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        assertNull(view.optJSONObject(JsonFormat.Keys.CHILDREN));
    }

    @Test
    public void noResultsIfInexactMatch() throws Exception {
        atom1 = createAtomWithTitle("earth");
        atom2 = createAtomWithTitle("Earth");
        atom3 = createAtomWithTitle("ear th");

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        assertNull(view.optJSONObject(JsonFormat.Keys.CHILDREN));
    }

    @Test
    public void resultsForExactMatch() throws Exception {
        topicGraph.begin();
        atom1 = createAtomWithTitle("earth");
        atom2 = createAtomWithTitle("earth");
        atom3 = createAtomWithTitle("earth");
        topicGraph.commit();

        Note atom = topicGraph.getNotesById(atom1.getId()).get();
        assertEquals("earth", atom.getTitle());
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
