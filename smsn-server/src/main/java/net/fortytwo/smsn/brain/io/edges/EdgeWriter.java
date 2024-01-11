package net.fortytwo.smsn.brain.io.edges;

import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.Filter;

import java.io.IOException;
import java.io.PrintStream;
import java.util.Collections;
import java.util.List;

public class EdgeWriter extends NoteWriter {

    @Override
    public List<Format> getFormats() {
        return Collections.singletonList(EdgeTSVFormat.getInstance());
    }

    @Override
    public void doWrite(Context context) throws IOException {
        TopicGraph sourceGraph = context.getTopicGraph();
        PrintStream p = new PrintStream(context.getDestStream());
        Filter filter = context.getFilter();

        p.println("from\tto");

        for (Note fromNote : sourceGraph.getAllNotes()) {
            if (null != fromNote && filter.test(fromNote)) {
                ListNode<Note> l = fromNote.getChildren();
                while (null != l) {
                    Note toNote = l.getFirst();
                    if (filter.test(toNote)) {
                        printEdge(p, fromNote, toNote);
                    }
                    l = l.getRest();
                }
            }
        }
    }

    private void printEdge(final PrintStream p, final Note fromNote, final Note toNote) {
        p.print(Note.getId(fromNote));
        p.print('\t');
        p.print(Note.getId(toNote));
        p.print('\n');
    }
}
