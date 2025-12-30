package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Params;
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
import static org.junit.Assert.assertTrue;

/**
 * Tests for the UpdateView action.
 * UpdateView is the core action for modifying the graph through the editor.
 */
public class UpdateViewTest extends ActionTestBase {
    private AtomRepository repository;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
    }

    @Test
    public void updateViewAddsChildToNote() throws Exception {
        Atom root = createAtom("root");
        Atom newChild = createAtom("new child");

        // Wiki format: child bullet point with :id: format
        String wikiContent = "* :" + newChild.id.value + ": new child";

        ActionContext context = perform(createWikiUpdateAction(root.id, wikiContent, 1));
        JSONObject view = getView(context);
        assertNotNull(view);

        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertNotNull(children);
        assertEquals(1, children.length());
        assertEquals(newChild.id.value, children.getJSONObject(0).getString(JsonFormat.Keys.ID));
    }

    @Test
    public void updateViewRemovesAllChildren() throws Exception {
        Atom root = createAtom("root");
        Atom child = createAtom("child");
        repository.addChildAt(root.id, child.id, 0);

        // Empty content removes all children
        ActionContext context = perform(createWikiUpdateAction(root.id, "", 1));
        JSONObject view = getView(context);
        assertNotNull(view);

        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertTrue(children == null || children.length() == 0);
    }

    @Test
    public void updateViewPreservesChildOrder() throws Exception {
        Atom root = createAtom("root");
        Atom first = createAtom("first");
        Atom second = createAtom("second");
        Atom third = createAtom("third");

        String wikiContent = "* :" + first.id.value + ": first\n" +
                             "* :" + second.id.value + ": second\n" +
                             "* :" + third.id.value + ": third";

        ActionContext context = perform(createWikiUpdateAction(root.id, wikiContent, 1));
        JSONObject view = getView(context);

        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertEquals(3, children.length());
        assertEquals("first", children.getJSONObject(0).getString(SemanticSynchrony.PropertyKeys.TITLE));
        assertEquals("second", children.getJSONObject(1).getString(SemanticSynchrony.PropertyKeys.TITLE));
        assertEquals("third", children.getJSONObject(2).getString(SemanticSynchrony.PropertyKeys.TITLE));
    }

    @Test
    public void updateViewCreatesNewNote() throws Exception {
        Atom root = createAtom("root");

        // Bullet without :id: format creates a new note
        String wikiContent = "* brand new note";

        ActionContext context = perform(createWikiUpdateAction(root.id, wikiContent, 1));
        JSONObject view = getView(context);

        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertNotNull(children);
        assertEquals(1, children.length());
        assertEquals("brand new note", children.getJSONObject(0).getString(SemanticSynchrony.PropertyKeys.TITLE));
    }

    @Test
    public void updateViewReturnsUpdatedView() throws Exception {
        Atom root = createAtom("root");

        ActionContext context = perform(createWikiUpdateAction(root.id, "", 1));

        assertNotNull(context.getMap().get(Params.VIEW));
    }

    @Test
    public void updateViewReordersChildren() throws Exception {
        Atom root = createAtom("root");
        Atom first = createAtom("first");
        Atom second = createAtom("second");
        Atom third = createAtom("third");

        // Add in one order
        repository.addChildAt(root.id, first.id, 0);
        repository.addChildAt(root.id, second.id, 1);
        repository.addChildAt(root.id, third.id, 2);

        // Update with different order using :id: format
        String wikiContent = "* :" + third.id.value + ": third\n" +
                             "* :" + first.id.value + ": first\n" +
                             "* :" + second.id.value + ": second";

        ActionContext context = perform(createWikiUpdateAction(root.id, wikiContent, 1));
        JSONObject view = getView(context);

        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertEquals(3, children.length());
        assertEquals("third", children.getJSONObject(0).getString(SemanticSynchrony.PropertyKeys.TITLE));
        assertEquals("first", children.getJSONObject(1).getString(SemanticSynchrony.PropertyKeys.TITLE));
        assertEquals("second", children.getJSONObject(2).getString(SemanticSynchrony.PropertyKeys.TITLE));
    }

    @Test
    public void updateViewRemovesSpecificChild() throws Exception {
        Atom root = createAtom("root");
        Atom keep1 = createAtom("keep1");
        Atom remove = createAtom("remove");
        Atom keep2 = createAtom("keep2");

        repository.addChildAt(root.id, keep1.id, 0);
        repository.addChildAt(root.id, remove.id, 1);
        repository.addChildAt(root.id, keep2.id, 2);

        // Update without the middle child using :id: format
        String wikiContent = "* :" + keep1.id.value + ": keep1\n" +
                             "* :" + keep2.id.value + ": keep2";

        ActionContext context = perform(createWikiUpdateAction(root.id, wikiContent, 1));
        JSONObject view = getView(context);

        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertEquals(2, children.length());
        assertEquals("keep1", children.getJSONObject(0).getString(SemanticSynchrony.PropertyKeys.TITLE));
        assertEquals("keep2", children.getJSONObject(1).getString(SemanticSynchrony.PropertyKeys.TITLE));
    }

    private Atom createAtom(String title) {
        AtomId id = SemanticSynchrony.createRandomId();
        return repository.createAtom(id, new SourceName("private"), title);
    }

    private UpdateView createWikiUpdateAction(AtomId rootId, String content, int height) {
        UpdateView action = new UpdateView();
        action.setRoot(rootId.value);
        action.setHeight(height);
        action.setFilter(Filter.noFilter());
        action.setView(content);
        action.setViewFormat(Params.Format.wiki);
        return action;
    }
}
