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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

public class FindIsolatedNotesTest extends ActionTestBase {
    private AtomRepository repository;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
    }

    @Test
    public void emptyGraphReturnsEmptyResult() throws Exception {
        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        // Empty graph should have no children
        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertTrue(children == null || children.length() == 0);
    }

    @Test
    public void singleNoteWithNoConnectionsIsIsolated() throws Exception {
        createAtom("lonely note");

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        assertEquals(1, view.optJSONArray(JsonFormat.Keys.CHILDREN).length());
    }

    @Test
    public void noteWithChildrenIsNotIsolated() throws Exception {
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");
        repository.addChildAt(parent.id, child.id, 0);

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        // Neither parent (has children) nor child (has parent) is isolated
        assertNull(view.optJSONArray(JsonFormat.Keys.CHILDREN));
    }

    @Test
    public void multipleIsolatedNotesAreAllReturned() throws Exception {
        createAtom("isolated one");
        createAtom("isolated two");
        createAtom("isolated three");

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        assertEquals(3, view.optJSONArray(JsonFormat.Keys.CHILDREN).length());
    }

    @Test
    public void mixedGraphReturnsOnlyIsolatedNotes() throws Exception {
        // Create connected notes
        Atom parent = createAtom("connected parent");
        Atom child = createAtom("connected child");
        repository.addChildAt(parent.id, child.id, 0);

        // Create isolated notes
        createAtom("isolated one");
        createAtom("isolated two");

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        assertEquals(2, view.optJSONArray(JsonFormat.Keys.CHILDREN).length());
    }

    @Test
    public void titleIsSetCorrectly() throws Exception {
        // Need at least one atom for the action to succeed
        createAtom("test");
        ActionContext context = perform(createAction());
        assertEquals("isolated notes", context.getMap().get("title"));
    }

    private Atom createAtom(String title) {
        AtomId id = SemanticSynchrony.createRandomId();
        return repository.createAtom(id, new SourceName("private"), title);
    }

    private FindIsolatedNotes createAction() {
        FindIsolatedNotes action = new FindIsolatedNotes();
        action.setFilter(Filter.noFilter());
        return action;
    }
}
