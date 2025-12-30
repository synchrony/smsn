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

public class FindRootsTest extends ActionTestBase {
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
    public void singleNoteWithNoParentIsRoot() throws Exception {
        createAtom("root note");

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        assertEquals(1, view.optJSONArray(JsonFormat.Keys.CHILDREN).length());
    }

    @Test
    public void noteWithParentIsNotRoot() throws Exception {
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");
        repository.addChildAt(parent.id, child.id, 0);

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        // Only parent is a root; child has a parent
        assertEquals(1, view.optJSONArray(JsonFormat.Keys.CHILDREN).length());
    }

    @Test
    public void multipleRootsAreAllReturned() throws Exception {
        createAtom("root one");
        createAtom("root two");
        createAtom("root three");

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        assertEquals(3, view.optJSONArray(JsonFormat.Keys.CHILDREN).length());
    }

    @Test
    public void deepTreeHasOneRoot() throws Exception {
        Atom grandparent = createAtom("grandparent");
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");

        repository.addChildAt(grandparent.id, parent.id, 0);
        repository.addChildAt(parent.id, child.id, 0);

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        assertEquals(1, view.optJSONArray(JsonFormat.Keys.CHILDREN).length());
    }

    @Test
    public void noteWithChildrenButNoParentIsRoot() throws Exception {
        Atom parent = createAtom("root with children");
        Atom child1 = createAtom("child 1");
        Atom child2 = createAtom("child 2");

        repository.addChildAt(parent.id, child1.id, 0);
        repository.addChildAt(parent.id, child2.id, 1);

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);
        // Parent is a root (no parents), children are not (they have parent)
        assertEquals(1, view.optJSONArray(JsonFormat.Keys.CHILDREN).length());
    }

    @Test
    public void titleIsSetCorrectly() throws Exception {
        // Need at least one atom for the action to succeed
        createAtom("test");
        ActionContext context = perform(createAction());
        assertEquals("all roots", context.getMap().get("title"));
    }

    private Atom createAtom(String title) {
        AtomId id = SemanticSynchrony.createRandomId();
        return repository.createAtom(id, new SourceName("private"), title);
    }

    private FindRoots createAction() {
        FindRoots action = new FindRoots();
        action.setFilter(Filter.noFilter());
        action.setHeight(1);
        return action;
    }
}
