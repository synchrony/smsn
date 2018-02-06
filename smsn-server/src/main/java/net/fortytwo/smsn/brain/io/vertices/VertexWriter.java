package net.fortytwo.smsn.brain.io.vertices;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.rdf.KnowledgeBase;

import java.io.IOException;
import java.io.PrintStream;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

public class VertexWriter extends NoteWriter {
    private static final Logger logger = Logger.getLogger(VertexWriter.class.getName());

    @Override
    public List<Format> getFormats() {
        return Collections.singletonList(VertexTSVFormat.getInstance());
    }

    @Override
    public void doWrite(Context context) throws IOException {

        TopicGraph sourceGraph = context.getTopicGraph();
        Filter filter = context.getFilter();
        Preconditions.checkNotNull(filter);
        KnowledgeBase sourceKb = context.getKnowledgeBase();
        PrintStream p = new PrintStream(context.getDestStream());

        p.println("created\tid\tweight\tpriority\tsource\tclass\tout\tin\ttitle\talias\tshortcut");

        for (Note a : sourceGraph.getAllNotes()) {
            if (isTrueNote(a) && filter.test(a)) {
                p.print(Note.getCreated(a));
                p.print('\t');
                p.print(Note.getId(a));
                p.print('\t');
                p.print(Note.getWeight(a));
                p.print('\t');
                p.print(Note.getPriority(a));
                p.print('\t');
                p.print(Note.getSource(a));
                p.print('\t');

                List<KnowledgeBase.NoteClassEntry> entries = sourceKb.getClassInfo(a);
                if (null != entries && entries.size() > 0) {
                    KnowledgeBase.NoteClassEntry e = entries.get(0);
                    p.print(e.getInferredClassName());
                    p.print('\t');
                    p.print(e.getOutScore());
                    p.print('\t');
                    p.print(e.getInScore());
                    p.print('\t');
                } else {
                    p.print("\t0\t0\t");
                }

                String title = Note.getTitle(a);
                if (null == title) {
                    logger.warning("note has null @title: " + Note.getId(a));
                } else {
                    p.print(escapeValue(title));
                }
                p.print('\t');

                String alias = Note.getAlias(a);
                if (null != alias) {
                    p.print(escapeValue(alias));
                }
                p.print('\t');

                String shortcut = Note.getShortcut(a);
                if (null != shortcut) {
                    p.print(escapeValue(shortcut));
                }

                p.print('\n');
            }
        }
    }

    private boolean isTrueNote(final Note a) {
        return null != Note.getCreated(a);
    }

    // Note: quote characters (") need to be replaced, e.g. with underscores (_), if this data is imported into R.
    // Otherwise, R becomes confused and skips rows.
    private String escapeValue(final String value) {
        return SemanticSynchrony.unicodeEscape(value);
    }
}
