package net.fortytwo.smsn.brain.io.edges;

import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.AtomList;

import java.io.IOException;
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
    public void doExport(Context context) throws IOException {
        AtomGraph sourceGraph = context.getFilteredGraph();
        PrintStream p = new PrintStream(context.getDestStream());

        p.println("from\tto");

        for (Atom a : sourceGraph.getAllAtoms()) {
            if (null != a) {
                AtomList l = a.getNotes();
                while (null != l) {
                    p.print(a.getId());
                    p.print('\t');
                    p.print(l.getFirst().getId());
                    p.print('\n');
                    l = l.getRest();
                }
            }
        }
    }
}
