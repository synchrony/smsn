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

public class GetViewTest extends ActionTestBase {
    private AtomRepository repository;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
    }

    @Test
    public void getViewReturnsRootAtom() throws Exception {
        Atom root = createAtom("root note");

        ActionContext context = perform(createAction(root.id, 1));
        JSONObject view = getView(context);

        assertNotNull(view);
        assertEquals(root.id.value, view.getString(JsonFormat.Keys.ID));
        assertEquals("root note", view.getString(SemanticSynchrony.PropertyKeys.TITLE));
    }

    @Test
    public void getViewWithHeight0ReturnsNoChildren() throws Exception {
        Atom root = createAtom("root");
        Atom child = createAtom("child");
        repository.addChildAt(root.id, child.id, 0);

        ActionContext context = perform(createAction(root.id, 0));
        JSONObject view = getView(context);

        assertNotNull(view);
        assertEquals(root.id.value, view.getString(JsonFormat.Keys.ID));
        // Height 0 should not expand children
        assertNull(view.optJSONArray(JsonFormat.Keys.CHILDREN));
    }

    @Test
    public void getViewWithHeight1ReturnsDirectChildren() throws Exception {
        Atom root = createAtom("root");
        Atom child1 = createAtom("child 1");
        Atom child2 = createAtom("child 2");
        repository.addChildAt(root.id, child1.id, 0);
        repository.addChildAt(root.id, child2.id, 1);

        ActionContext context = perform(createAction(root.id, 1));
        JSONObject view = getView(context);

        assertNotNull(view);
        assertEquals(root.id.value, view.getString(JsonFormat.Keys.ID));

        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertNotNull(children);
        assertEquals(2, children.length());
    }

    @Test
    public void getViewPreservesChildOrder() throws Exception {
        Atom root = createAtom("root");
        Atom first = createAtom("first");
        Atom second = createAtom("second");
        Atom third = createAtom("third");

        repository.addChildAt(root.id, first.id, 0);
        repository.addChildAt(root.id, second.id, 1);
        repository.addChildAt(root.id, third.id, 2);

        ActionContext context = perform(createAction(root.id, 1));
        JSONObject view = getView(context);

        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertEquals("first", children.getJSONObject(0).getString(SemanticSynchrony.PropertyKeys.TITLE));
        assertEquals("second", children.getJSONObject(1).getString(SemanticSynchrony.PropertyKeys.TITLE));
        assertEquals("third", children.getJSONObject(2).getString(SemanticSynchrony.PropertyKeys.TITLE));
    }

    @Test
    public void getViewIncludesNumberOfChildren() throws Exception {
        Atom root = createAtom("root");
        Atom child1 = createAtom("child 1");
        Atom child2 = createAtom("child 2");
        repository.addChildAt(root.id, child1.id, 0);
        repository.addChildAt(root.id, child2.id, 1);

        ActionContext context = perform(createAction(root.id, 1));
        JSONObject view = getView(context);

        assertEquals(2, view.getInt(JsonFormat.Keys.NUMBER_OF_CHILDREN));
    }

    @Test
    public void getViewIncludesNumberOfParents() throws Exception {
        Atom root1 = createAtom("root 1");
        Atom root2 = createAtom("root 2");
        Atom child = createAtom("shared child");

        repository.addChildAt(root1.id, child.id, 0);
        repository.addChildAt(root2.id, child.id, 0);

        ActionContext context = perform(createAction(child.id, 1));
        JSONObject view = getView(context);

        assertEquals(2, view.getInt(JsonFormat.Keys.NUMBER_OF_PARENTS));
    }

    @Test
    public void getViewWithDeepHierarchy() throws Exception {
        Atom grandparent = createAtom("grandparent");
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");

        repository.addChildAt(grandparent.id, parent.id, 0);
        repository.addChildAt(parent.id, child.id, 0);

        // Height 2 should show grandparent -> parent -> child
        ActionContext context = perform(createAction(grandparent.id, 2));
        JSONObject view = getView(context);

        JSONArray level1 = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertNotNull(level1);
        assertEquals(1, level1.length());
        assertEquals("parent", level1.getJSONObject(0).getString(SemanticSynchrony.PropertyKeys.TITLE));

        JSONArray level2 = level1.getJSONObject(0).optJSONArray(JsonFormat.Keys.CHILDREN);
        assertNotNull(level2);
        assertEquals(1, level2.length());
        assertEquals("child", level2.getJSONObject(0).getString(SemanticSynchrony.PropertyKeys.TITLE));
    }

    private Atom createAtom(String title) {
        AtomId id = SemanticSynchrony.createRandomId();
        return repository.createAtom(id, new SourceName("private"), title);
    }

    private GetView createAction(AtomId rootId, int height) {
        GetView action = new GetView();
        action.setRoot(rootId.value);
        action.setHeight(height);
        action.setFilter(Filter.noFilter());
        return action;
    }
}
