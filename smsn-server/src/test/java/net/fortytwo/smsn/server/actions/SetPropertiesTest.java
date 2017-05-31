package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class SetPropertiesTest extends ActionTestBase {
    private Atom atom;

    @Before
    public void setUp() throws Exception {
        super.setUp();

        topicGraph.begin();
        atom = createAtomWithTitle("before");
        atom.setText("the page");
        assertEquals(1, countAtoms());
        topicGraph.commit();

        atom = topicGraph.getAtomById(atom.getId()).get();
        assertEquals("before", atom.getTitle());
        assertEquals("the page", atom.getText());

        assertEquals(1, countAtoms());
    }

    @Test
    public void titleIsSetCorrectly() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.PropertyKeys.TITLE);
        action.setValue("after");

        perform(action);

        atom = topicGraph.getAtomById(atom.getId()).get();
        assertEquals("after", atom.getTitle());
    }

    @Test(expected = RequestProcessingException.class)
    public void emptyTitleIsError() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.PropertyKeys.TITLE);
        action.setValue("  \n");

        perform(action);
    }

    @Test
    public void pageIsSetCorrectly() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.PropertyKeys.TEXT);
        action.setValue("after");

        perform(action);

        atom = topicGraph.getAtomById(atom.getId()).get();
        assertEquals("after", atom.getText());
    }

    @Test
    public void emptyPageBecomesNullPage() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.PropertyKeys.TEXT);
        action.setValue("  \n ");

        perform(action);

        atom = topicGraph.getAtomById(atom.getId()).get();
        assertNull(atom.getText());
    }

    @Test
    public void weightIsSetCorrectly() throws Exception {
        topicGraph.begin();
        Atom atom = createAtomWithTitle("test");
        atom.setWeight(0.25f);
        topicGraph.commit();

        atom = topicGraph.getAtomById(atom.getId()).get();
        assertEquals(0.25f, atom.getWeight(), 0.0f);

        SetProperties action = createAction();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.PropertyKeys.WEIGHT);
        action.setValue(0.5);

        perform(action);

        atom = topicGraph.getAtomById(atom.getId()).get();
        assertEquals(0.5f, atom.getWeight(), 0.0f);
    }

    @Test
    public void sourceIsSetCorrectly() throws Exception {
        topicGraph.begin();
        Atom atom = createAtomWithTitle("test");
        atom.setSource(DefaultSources.PRIVATE);
        topicGraph.commit();

        atom = topicGraph.getAtomById(atom.getId()).get();
        assertEquals(DefaultSources.PRIVATE, atom.getSource());

        SetProperties action = createAction();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.PropertyKeys.SOURCE);
        action.setValue(DefaultSources.PERSONAL);

        perform(action);

        atom = topicGraph.getAtomById(atom.getId()).get();
        assertEquals(DefaultSources.PERSONAL, atom.getSource());
    }

    @Test
    public void priorityIsSetCorrectly() throws Exception {
        topicGraph.begin();
        Atom atom = createAtomWithTitle("test");
        atom.setPriority(0.25f);
        topicGraph.commit();

        atom = topicGraph.getAtomById(atom.getId()).get();
        assertEquals(0.25f, atom.getPriority(), 0.0f);

        SetProperties action = createAction();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.PropertyKeys.PRIORITY);
        action.setValue(0.5);

        perform(action);

        atom = topicGraph.getAtomById(atom.getId()).get();
        assertEquals(0.5f, atom.getPriority(), 0.0f);
    }

    @Test
    public void shortcutIsSetCorrectly() throws Exception {
        topicGraph.begin();
        Atom atom = createAtomWithTitle("test");
        topicGraph.commit();

        atom = topicGraph.getAtomById(atom.getId()).get();
        assertNull(atom.getShortcut());

        SetProperties action = createAction();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.PropertyKeys.SHORTCUT);
        action.setValue("after");

        perform(action);

        atom = topicGraph.getAtomById(atom.getId()).get();
        assertEquals("after", atom.getShortcut());
    }

    private SetProperties createAction() {
        SetProperties action = new SetProperties();
        action.setFilter(Filter.noFilter());
        return action;
    }
}
