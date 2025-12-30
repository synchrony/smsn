package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.io.json.JsonFormat;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.query.QueryType;
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
 * Tests for the Search action.
 * Note: TinkerGraph (used in tests) only supports exact title matching, not full-text search.
 * Full-text search with partial matching requires Neo4j with Lucene indexing.
 */
public class SearchTest extends ActionTestBase {
    private AtomRepository repository;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
    }

    @Test
    public void searchFindsExactTitleMatch() throws Exception {
        // TinkerGraph only supports exact matching
        createAtom("apple");
        createAtom("banana");
        createAtom("cherry");

        ActionContext context = perform(createAction("apple"));
        JSONObject view = getView(context);
        assertNotNull(view);

        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertNotNull("Expected children in search results", children);
        assertEquals(1, children.length());
    }

    @Test
    public void searchWithNoMatchesReturnsEmptyResult() throws Exception {
        createAtom("banana");
        createAtom("cherry");

        ActionContext context = perform(createAction("apple"));
        JSONObject view = getView(context);
        assertNotNull(view);

        // Empty result - no children array or empty children
        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertTrue(children == null || children.length() == 0);
    }

    @Test
    public void searchTitleIsSetToQuery() throws Exception {
        createAtom("test");

        ActionContext context = perform(createAction("test"));
        assertEquals("test", context.getMap().get("title"));
    }

    @Test
    public void searchResultsIncludeAtomProperties() throws Exception {
        Atom atom = createAtom("unique");

        ActionContext context = perform(createAction("unique"));
        JSONObject view = getView(context);
        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);

        assertNotNull(children);
        assertEquals(1, children.length());

        JSONObject result = children.getJSONObject(0);
        assertEquals("unique", result.getString(SemanticSynchrony.PropertyKeys.TITLE));
        assertEquals(atom.id.value, result.getString(JsonFormat.Keys.ID));
    }

    @Test
    public void searchWithAcronymQueryTypeDoesNotThrow() throws Exception {
        // Note: Acronym search may not work properly with TinkerGraph
        // This test just verifies the action executes without throwing
        createAtom("United States of America");

        ActionContext context = perform(createAction("usa", QueryType.Acronym));
        JSONObject view = getView(context);
        assertNotNull(view);
    }

    @Test
    public void searchWithShortcutQueryTypeDoesNotThrow() throws Exception {
        // Note: Shortcut search may not work properly with TinkerGraph
        // This test just verifies the action executes without throwing
        Atom atom = createAtom("Important Note");
        atom = atom.withShortcut(hydra.util.Opt.of("imp"));
        repository.save(atom);

        ActionContext context = perform(createAction("imp", QueryType.Shortcut));
        JSONObject view = getView(context);
        assertNotNull(view);
    }

    @Test
    public void multipleExactMatchesAreReturned() throws Exception {
        // Create two atoms with identical titles
        createAtom("duplicate title");
        createAtom("duplicate title");

        ActionContext context = perform(createAction("duplicate title"));
        JSONObject view = getView(context);
        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);

        assertNotNull(children);
        assertEquals(2, children.length());
    }

    private Atom createAtom(String title) {
        AtomId id = SemanticSynchrony.createRandomId();
        return repository.createAtom(id, new SourceName("private"), title);
    }

    private Search createAction(String query) {
        return createAction(query, QueryType.FullText);
    }

    private Search createAction(String query, QueryType queryType) {
        Search action = new Search();
        action.setQuery(query);
        action.setQueryType(queryType);
        action.setFilter(Filter.noFilter());
        action.setHeight(1);
        return action;
    }
}
