package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

public class GetAtomsByIdTest extends BrainTestBase {

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }

    @Test
    public void allLegalIdsMayBeUsed() throws Exception {
        String[] ids = new String[]{"aaaaaaa", "0000000", "a1b2c3d", "-------", "_______", "-_0-ad_"};

        int count = 0;
        for (String id : ids) {
            Note atom = createNote(id);
            atom.setTitle("atom #" + ++count);
        }

        for (int i = 0; i < ids.length; i++) {
            Note atom = topicGraph.getNotesById(ids[i]).get();
            assertEquals("atom #" + (i + 1), atom.getTitle());
        }
    }

    @Test
    public void testGetAtoms() throws Exception {
        Note chaos = createNote();
        chaos.setTitle("Chaos");
        Note tartarus = createNote();
        tartarus.setTitle("Tartarus");
        Note gaia = createNote();
        gaia.setTitle("Gaia");
        Note eros = createNote();
        eros.setTitle("Eros");
        Note nyx = createNote();
        nyx.setTitle("Nyx");
        Note erebus = createNote();
        erebus.setTitle("Erebus");
        ListNode<Note> children = topicGraph.createListOfNotes(tartarus, gaia, eros, nyx, erebus);
        chaos.setChildren(children);

        //System.out.println(chaos.getId());
        //System.out.println(chaos.getValue());

        // getAtoms returns a list of only atom vertices, excluding list vertices
        int count = 0;
        Iterable<Note> atoms = topicGraph.getAllNotes();
        for (Note a : atoms) {
            count++;
            //System.out.println(a.getId());
            //System.out.println(a.getValue());
        }
        assertEquals(6, count);
    }
}
