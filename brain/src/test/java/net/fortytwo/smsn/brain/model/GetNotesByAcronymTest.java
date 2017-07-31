package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.pg.PGNote;
import org.junit.Test;

import java.io.IOException;
import java.util.Collection;

import static org.junit.Assert.assertEquals;

public class GetNotesByAcronymTest extends BrainTestBase {

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createNeo4jTopicGraph();
    }

    @Test
    public void testAcronymSearch() throws Exception {
        Note a = createNote();
        Note.setTitle(a, "Arthur\tP.  Dent ");
        Note t = createNote();
        Note.setTitle(t, "Arthur's moth-eaten towel");
        Note l = createNote();
        Note.setTitle(l, "ooooooooo0ooooooooo1ooooooooo2ooooooooo3ooooooooo4ooooooooo5ooooooooo6ooooooooo7" +
                "ooooooooo8ooooooooo9oooooooooAoooooooooBoooooooooCoooooooooDoooooooooEoooooooooF");

        Collection<Note> result;

        // oops. This is not a full-text query.
        result = topicGraph.getNotesByAcronym("Arthur*", filter);
        assertEquals(0, result.size());

        // l has not been indexed because its value is too long
        result = topicGraph.getNotesByAcronym("o", filter);
        assertEquals(0, result.size());

        for (Note note : topicGraph.getAllNotes()) {
            System.out.println(Note.getId(note) + ": "
                    + ((PGNote) note).asVertex().property(SemanticSynchrony.PropertyKeys.ACRONYM));
        }

        // exact acronym match
        // capitalization, punctuation, and idiosyncrasies of white space are ignored
        result = topicGraph.getNotesByAcronym("apd", filter);
        assertEquals(1, result.size());
        assertEquals(Note.getId(a), Note.getId(result.iterator().next()));

        // hyphens and underscores are treated as white space, while apostrophes and other punctuation are ignored
        result = topicGraph.getNotesByAcronym("amet", filter);
        assertEquals(1, result.size());
        assertEquals(Note.getId(t), Note.getId(result.iterator().next()));

        // acronym search is case insensitive
        result = topicGraph.getNotesByAcronym("APD", filter);
        assertEquals(1, result.size());
        assertEquals(Note.getId(a), Note.getId(result.iterator().next()));
    }
}
