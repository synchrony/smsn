package net.fortytwo.smsn.server.io;

import com.tinkerpop.blueprints.Vertex;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomList;
import net.fortytwo.smsn.brain.BrainGraph;
import net.fortytwo.smsn.brain.ExtendoBrain;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class EdgeExporter extends Exporter {
    public static final String FORMAT = "Edges";

    @Override
    public List<String> getFormats() {
        return Arrays.asList(FORMAT);
    }

    @Override
    protected void exportInternal(ExtendoBrain sourceBrain, OutputStream destStream) throws IOException {
        BrainGraph sourceGraph = sourceBrain.getBrainGraph();
        PrintStream p = new PrintStream(destStream);

        p.println("from\tto");

        for (Vertex v : sourceGraph.getPropertyGraph().getVertices()) {
            Atom a = sourceGraph.getAtom(v);
            if (null != a) {
                AtomList l = a.getNotes();
                while (null != l) {
                    p.print(v.getId());
                    p.print('\t');
                    p.print(l.getFirst().asVertex().getId());
                    p.print('\n');
                    l = l.getRest();
                }
            }
        }
    }
}
