package net.fortytwo.smsn.server.io.edges;

import com.tinkerpop.blueprints.Vertex;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomList;
import net.fortytwo.smsn.brain.AtomGraph;
import net.fortytwo.smsn.brain.MyOtherBrain;
import net.fortytwo.smsn.server.io.BrainWriter;
import net.fortytwo.smsn.server.io.Format;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class EdgeWriter extends BrainWriter {

    @Override
    public List<Format> getFormats() {
        return Arrays.asList(EdgeTSVFormat.getInstance());
    }

    @Override
    protected void exportInternal(MyOtherBrain sourceBrain, OutputStream destStream, Format format) throws IOException {
        AtomGraph sourceGraph = sourceBrain.getAtomGraph();
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
