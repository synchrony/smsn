package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.Normed;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.io.json.JsonFormat;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.repository.AtomRepository;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Tests for GetPriorities action.
 * Note: The action uses the Brain obtained via Action.getBrain(wrapper),
 * so tests must add priorities through that same Brain instance.
 */
public class GetPrioritiesTest extends ActionTestBase {
    private AtomRepository repository;
    private Brain actionBrain;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
        // Get the brain that actions will use (via Action.getBrain)
        actionBrain = Action.getBrain(Action.getWrapper(graph));
    }

    @Test
    public void emptyGraphReturnsEmptyResult() throws Exception {
        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);

        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertTrue(children == null || children.length() == 0);
    }

    @Test
    public void returnsNoteWithPriority() throws Exception {
        Atom atom = createAtom("prioritized note");
        atom = atom.withPriority(hydra.util.Opt.of(new Normed(0.9f)));
        repository.save(atom);

        // Update priorities using the action's brain
        actionBrain.getPriorities().updatePriority(repository.load(atom.id));

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);
        assertNotNull(view);

        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertNotNull(children);
        assertEquals(1, children.length());
        assertEquals("prioritized note", children.getJSONObject(0).getString(SemanticSynchrony.PropertyKeys.TITLE));
    }

    @Test
    public void respectsMaxResultsLimit() throws Exception {
        // Create multiple prioritized notes
        for (int i = 0; i < 5; i++) {
            Atom atom = createAtom("note " + i);
            atom = atom.withPriority(hydra.util.Opt.of(new Normed(0.5f + i * 0.1f)));
            repository.save(atom);
            actionBrain.getPriorities().updatePriority(repository.load(atom.id));
        }

        GetPriorities action = createAction();
        action.setMaxResults(3);
        ActionContext context = perform(action);
        JSONObject view = getView(context);

        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertNotNull(children);
        assertEquals(3, children.length());
    }

    @Test(expected = IllegalArgumentException.class)
    public void rejectsZeroMaxResults() {
        GetPriorities action = new GetPriorities();
        action.setMaxResults(0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void rejectsNegativeMaxResults() {
        GetPriorities action = new GetPriorities();
        action.setMaxResults(-1);
    }

    @Test
    public void noteWithoutPriorityNotIncluded() throws Exception {
        // Create note without priority
        createAtom("no priority");

        ActionContext context = perform(createAction());
        JSONObject view = getView(context);

        JSONArray children = view.optJSONArray(JsonFormat.Keys.CHILDREN);
        assertTrue(children == null || children.length() == 0);
    }

    private Atom createAtom(String title) {
        AtomId id = SemanticSynchrony.createRandomId();
        return repository.createAtom(id, new SourceName("private"), title);
    }

    private GetPriorities createAction() {
        GetPriorities action = new GetPriorities();
        action.setFilter(Filter.noFilter());
        return action;
    }
}
