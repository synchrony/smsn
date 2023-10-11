package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

public class GetNotesByIdTest extends BrainTestBase {

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }

    @Test
    public void allLegalIdsMayBeUsed() throws Exception {
        AtomId[] ids = new AtomId[]{
                new AtomId("aaaaaaa"),
                new AtomId("0000000"),
                new AtomId("a1b2c3d"),
                new AtomId("-------"),
                new AtomId("_______"),
                new AtomId("-_0-ad_")};

        int count = 0;
        for (AtomId id : ids) {
            Note note = createNote(id);
            Note.setTitle(note, "note #" + ++count);
        }

        for (int i = 0; i < ids.length; i++) {
            Note note = topicGraph.getNoteById(ids[i]).get();
            assertEquals("note #" + (i + 1), Note.getTitle(note));
        }
    }

    @Test
    public void testGetNotes() throws Exception {
        Note chaos = createNote();
        Note.setTitle(chaos, "Chaos");
        Note tartarus = createNote();
        Note.setTitle(tartarus, "Tartarus");
        Note gaia = createNote();
        Note.setTitle(gaia, "Gaia");
        Note eros = createNote();
        Note.setTitle(eros, "Eros");
        Note nyx = createNote();
        Note.setTitle(nyx, "Nyx");
        Note erebus = createNote();
        Note.setTitle(erebus, "Erebus");
        ListNode<Note> children = topicGraph.createListOfNotes(tartarus, gaia, eros, nyx, erebus);
        chaos.setChildren(children);

        //System.out.println(chaos.getId());
        //System.out.println(chaos.getValue());

        // getNotes returns a list of only note vertices, excluding list vertices
        int count = 0;
        Iterable<Note> notes = topicGraph.getAllNotes();
        for (Note ignored : notes) {
            count++;
            //System.out.println(a.getId());
            //System.out.println(a.getValue());
        }
        assertEquals(6, count);
    }
}
