package net.fortytwo.smsn.brain.io.edges;

import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomList;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;

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
    protected void exportInternal(Brain sourceBrain, OutputStream destStream, Format format) throws IOException {
        AtomGraph sourceGraph = sourceBrain.getAtomGraph();
        PrintStream p = new PrintStream(destStream);

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
