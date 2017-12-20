package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.pg.PGNote;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

public class GetNotesByAcronymTest extends BrainTestBase {

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createNeo4jTopicGraph();
    }

    @Test
    public void testAcronymSearch() throws Exception {
        Note a = createNote();
        a.setLabel("Arthur\tP.  Dent ");
        Note t = createNote();
        t.setLabel("Arthur's moth-eaten towel");
        Note l = createNote();
        l.setLabel("ooooooooo0ooooooooo1ooooooooo2ooooooooo3ooooooooo4ooooooooo5ooooooooo6ooooooooo7" +
                "ooooooooo8ooooooooo9oooooooooAoooooooooBoooooooooCoooooooooDoooooooooEoooooooooF");

        Iterable<Note> result;

        // oops. This is not a full-text query.
        result = topicGraph.getNotesByAcronym("Arthur*", filter);
        assertEquals(0, count(result));

        // l has not been indexed because its value is too long
        result = topicGraph.getNotesByAcronym("o", filter);
        assertEquals(0, count(result));

        for (Note note : topicGraph.getAllNotes()) {
            System.out.println(note.getTopic().getId() + ": "
                    + ((PGNote) note).asVertex().property(SemanticSynchrony.PropertyKeys.ACRONYM));
        }

        // exact acronym match
        // capitalization, punctuation, and idiosyncrasies of white space are ignored
        result = topicGraph.getNotesByAcronym("apd", filter);
        assertEquals(1, count(result));
        assertEquals(a.getTopic().getId(), result.iterator().next().getTopic().getId());

        // hyphens and underscores are treated as white space, while apostrophes and other punctuation are ignored
        result = topicGraph.getNotesByAcronym("amet", filter);
        assertEquals(1, count(result));
        assertEquals(t.getTopic().getId(), result.iterator().next().getTopic().getId());

        // acronym search is case insensitive
        result = topicGraph.getNotesByAcronym("APD", filter);
        assertEquals(1, count(result));
        assertEquals(a.getTopic().getId(), result.iterator().next().getTopic().getId());
    }
}
