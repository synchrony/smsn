package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.junit.Test;

import java.io.IOException;
import java.util.Iterator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class GetNotesByIdTest extends BrainTestBase {

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }

    @Test
    public void allLegalIdsMayBeUsed() throws Exception {
        String[] ids = new String[]{"aaaaaaa", "0000000", "a1b2c3d", "-------", "_______", "-_0-ad_"};

        int count = 0;
        for (String id : ids) {
            Note note = createNote(id);
            note.setLabel("note #" + ++count);
        }

        for (int i = 0; i < ids.length; i++) {
            Topic topic = topicGraph.getTopicById(ids[i]).get();
            Iterator<Note> notes = topic.getNotes();
            assertTrue(notes.hasNext());
            assertEquals("note #" + (i + 1), notes.next().getLabel());
        }
    }

    @Test
    public void testGetNotes() throws Exception {
        Note chaos = createNote();
        chaos.setLabel("Chaos");
        Note tartarus = createNote();
        tartarus.setLabel("Tartarus");
        Note gaia = createNote();
        gaia.setLabel("Gaia");
        Note eros = createNote();
        eros.setLabel("Eros");
        Note nyx = createNote();
        nyx.setLabel("Nyx");
        Note erebus = createNote();
        erebus.setLabel("Erebus");
        Note.setChildren(chaos, tartarus, gaia, eros, nyx, erebus);

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
