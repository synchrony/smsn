package net.fortytwo.smsn.brain.io.vertices;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.rdf.KnowledgeBase;

import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

public class VertexWriter extends BrainWriter {
    private static final Logger logger = Logger.getLogger(VertexWriter.class.getName());

    @Override
    public List<Format> getFormats() {
        return Arrays.asList(VertexTSVFormat.getInstance());
    }

    @Override
    public void doExport(Context context) throws IOException {

        TopicGraph sourceGraph = context.getTopicGraph();
        Filter filter = context.getFilter();
        Preconditions.checkNotNull(filter);
        KnowledgeBase sourceKb = context.getKnowledgeBase();
        PrintStream p = new PrintStream(context.getDestStream());

        p.println("created\tid\tweight\tsource\tclass\tout\tin\tvalue\talias");

        for (Atom a : sourceGraph.getAllAtoms()) {
            if (isTrueAtom(a) && filter.test(a)) {
                p.print(a.getCreated());
                p.print('\t');
                p.print(a.getId());
                p.print('\t');
                p.print(a.getWeight());
                p.print('\t');
                p.print(a.getSource());
                p.print('\t');

                List<KnowledgeBase.AtomClassEntry> entries = sourceKb.getClassInfo(a);
                if (null != entries && entries.size() > 0) {
                    KnowledgeBase.AtomClassEntry e = entries.get(0);
                    p.print(e.getInferredClassName());
                    p.print('\t');
                    p.print(e.getOutScore());
                    p.print('\t');
                    p.print(e.getInScore());
                    p.print('\t');
                } else {
                    p.print("\t0\t0\t");
                }

                String value = a.getTitle();
                if (null == value) {
                    logger.warning("note has null @value: " + a.getId());
                } else {
                    p.print(escapeValue(a.getTitle()));
                }
                p.print('\t');

                String alias = a.getAlias();
                if (null != alias) {
                    p.print(escapeValue(alias));
                }

                p.print('\n');
            }
        }
    }

    private boolean isTrueAtom(final Atom a) {
        return null != a.getCreated();
    }

    // Note: quote characters (") need to be replaced, e.g. with underscores (_), if this data is imported into R.
    // Otherwise, R becomes confused and skips rows.
    private String escapeValue(final String value) {
        return SemanticSynchrony.unicodeEscape(value);
    }
}
