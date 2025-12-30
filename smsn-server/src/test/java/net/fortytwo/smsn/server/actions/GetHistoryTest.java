package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.io.json.JsonFormat;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.repository.AtomRepository;
import net.fortytwo.smsn.server.ActionContext;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Tests for GetHistory action.
 * Note: History is a static singleton shared across tests, and may contain
 * stale entries from atoms created in previous tests with different graphs.
 * Tests should be designed to work with potentially stale history entries.
 */
public class GetHistoryTest extends ActionTestBase {
    private AtomRepository repository;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
    }

    @Test
    public void returnsViewWithChildren() throws Exception {
        // Create and visit a note
        Atom atom = createAtom("test note for history");
        visitNote(atom.id);

        // Get history - note: history may contain stale entries from previous tests
        // that will be filtered out because those atoms don't exist in this graph
        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);

        // History should return a valid view structure
        // The children may be empty if all historical entries are from different graphs
        // But at minimum, we should have the note we just visited
        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertNotNull("Expected children array in history view", children);
        assertTrue("Expected at least one entry in history", children.length() >= 1);

        // The most recently visited note should be first
        String firstTitle = children.getJSONObject(0).getString(SemanticSynchrony.PropertyKeys.TITLE);
        assertEquals("test note for history", firstTitle);
    }

    @Test
    public void historyReturnsValidViewStructure() throws Exception {
        // This test verifies the basic structure of the history response
        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull("GetHistory should return a view", view);

        // The view may have children array (possibly empty or with valid atoms)
        // This just verifies the action completes successfully
    }

    private Atom createAtom(String title) {
        AtomId id = SemanticSynchrony.createRandomId();
        return repository.createAtom(id, new SourceName("private"), title);
    }

    private void visitNote(AtomId id) {
        GetView getView = new GetView();
        getView.setRoot(id.value);
        getView.setHeight(0);
        getView.setFilter(Filter.noFilter());
        perform(getView);
    }

    private GetHistory createAction() {
        GetHistory action = new GetHistory();
        action.setFilter(Filter.noFilter());
        return action;
    }
}
