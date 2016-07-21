package net.fortytwo.smsn.server.io;

import com.tinkerpop.blueprints.Vertex;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomGraph;
import net.fortytwo.smsn.brain.MyOtherBrain;
import net.fortytwo.smsn.brain.rdf.KnowledgeBase;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class VertexExporter extends Exporter {
    protected static final Logger logger = Logger.getLogger(VertexExporter.class.getName());

    public static final String FORMAT = "Vertices";

    @Override
    public List<String> getFormats() {
        return Arrays.asList(FORMAT);
    }

    @Override
    protected void exportInternal(MyOtherBrain sourceBrain, OutputStream destStream) throws IOException {
        AtomGraph sourceGraph = sourceBrain.getAtomGraph();
        KnowledgeBase sourceKb = sourceBrain.getKnowledgeBase();
        PrintStream p = new PrintStream(destStream);

        p.println("created\tid\tweight\tsharability\tclass\tout\tin\tvalue\talias");

        for (Vertex v : sourceGraph.getPropertyGraph().getVertices()) {
            Object c = v.getProperty(SemanticSynchrony.CREATED);
            if (null != c) {
                p.print(c);
                p.print('\t');
                p.print(v.getId());
                p.print('\t');
                p.print((double) v.getProperty(SemanticSynchrony.WEIGHT));
                p.print('\t');
                p.print((double) v.getProperty(SemanticSynchrony.SHARABILITY));
                p.print('\t');

                List<KnowledgeBase.AtomClassEntry> entries = sourceKb.getClassInfo(sourceGraph.getAtom(v));
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

                String value = v.getProperty(SemanticSynchrony.VALUE);
                if (null == value) {
                    logger.warning("note has null @value: " + v.getId());
                } else {
                    p.print(escapeValue(v.getProperty(SemanticSynchrony.VALUE)));
                }
                p.print('\t');

                String alias = v.getProperty(SemanticSynchrony.ALIAS);
                if (null != alias) {
                    p.print(escapeValue(alias));
                }

                p.print('\n');
            }
        }
    }

    // Note: quote characters (") need to be replaced, e.g. with underscores (_), if this data is imported into R.
    // Otherwise, R becomes confused and skips rows.
    private String escapeValue(final String value) {
        return SemanticSynchrony.unicodeEscape(value);
    }
}
