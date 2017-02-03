package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.server.errors.BadRequestException;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class SetPropertiesTest extends ActionTestBase {
    private Atom atom;

    @Before
    public void setUp() throws Exception {
        super.setUp();

        atomGraph.begin();
        atom = atomGraph.createAtom(null);
        atom.setTitle("before");
        atom.setPage("the page");
        atomGraph.commit();

        atom = atomGraph.getAtomById(atom.getId());
        assertEquals("before", atom.getTitle());
        assertEquals("the page", atom.getPage());
    }

    @Test
    public void titleIsSetCorrectly() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.TITLE);
        action.setValue("after");

        perform(action);

        atom = atomGraph.getAtomById(atom.getId());
        assertEquals("after", atom.getTitle());
    }

    @Test(expected = BadRequestException.class)
    public void emptyTitleIsError() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.TITLE);
        action.setValue("  \n");

        perform(action);
    }

    @Test
    public void pageIsSetCorrectly() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.PAGE);
        action.setValue("after");

        perform(action);

        atom = atomGraph.getAtomById(atom.getId());
        assertEquals("after", atom.getPage());
    }

    @Test
    public void emptyPageBecomesNullPage() throws Exception {

        SetProperties action = new SetProperties();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.PAGE);
        action.setValue("  \n ");

        perform(action);

        atom = atomGraph.getAtomById(atom.getId());
        assertNull(atom.getPage());
    }

    @Test
    public void weightIsSetCorrectly() throws Exception {
        atomGraph.begin();
        Atom atom = atomGraph.createAtom(null);
        atom.setTitle("test");
        atom.setWeight(0.25f);
        atomGraph.commit();

        atom = atomGraph.getAtomById(atom.getId());
        assertEquals(0.25f, atom.getWeight(), 0.0f);

        SetProperties action = new SetProperties();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.WEIGHT);
        action.setValue(0.5);

        perform(action);

        atom = atomGraph.getAtomById(atom.getId());
        assertEquals(0.5f, atom.getWeight(), 0.0f);
    }

    @Test
    public void sharabilityIsSetCorrectly() throws Exception {
        atomGraph.begin();
        Atom atom = atomGraph.createAtom(null);
        atom.setTitle("test");
        atom.setSharability(0.25f);
        atomGraph.commit();

        atom = atomGraph.getAtomById(atom.getId());
        assertEquals(0.25f, atom.getSharability(), 0.0f);

        SetProperties action = new SetProperties();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.SHARABILITY);
        action.setValue(0.5);

        perform(action);

        atom = atomGraph.getAtomById(atom.getId());
        assertEquals(0.5f, atom.getSharability(), 0.0f);
    }

    @Test
    public void priorityIsSetCorrectly() throws Exception {
        atomGraph.begin();
        Atom atom = atomGraph.createAtom(null);
        atom.setTitle("test");
        atom.setPriority(0.25f);
        atomGraph.commit();

        atom = atomGraph.getAtomById(atom.getId());
        assertEquals(0.25f, atom.getPriority(), 0.0f);

        SetProperties action = new SetProperties();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.PRIORITY);
        action.setValue(0.5);

        perform(action);

        atom = atomGraph.getAtomById(atom.getId());
        assertEquals(0.5f, atom.getPriority(), 0.0f);
    }

    @Test
    public void shortcutIsSetCorrectly() throws Exception {
        atomGraph.begin();
        Atom atom = atomGraph.createAtom(null);
        atom.setTitle("test");
        atomGraph.commit();

        atom = atomGraph.getAtomById(atom.getId());
        assertNull(atom.getShortcut());

        SetProperties action = new SetProperties();
        action.setId(atom.getId());
        action.setName(SemanticSynchrony.SHORTCUT);
        action.setValue("after");

        perform(action);

        atom = atomGraph.getAtomById(atom.getId());
        assertEquals("after", atom.getShortcut());
    }
}
