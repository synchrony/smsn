package net.fortytwo.smsn.brain.io.edges;

import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.model.Filter;

import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;

public class EdgeWriter extends BrainWriter {

    @Override
    public List<Format> getFormats() {
        return Arrays.asList(EdgeTSVFormat.getInstance());
    }

    @Override
    public void doExport(Context context) throws IOException {
        TopicGraph sourceGraph = context.getTopicGraph();
        PrintStream p = new PrintStream(context.getDestStream());
        Filter filter = context.getFilter();

        p.println("from\tto");

        for (Atom fromAtom : sourceGraph.getAllAtoms()) {
            if (null != fromAtom && filter.isVisible(fromAtom)) {
                EntityList<Atom> l = fromAtom.getNotes();
                while (null != l) {
                    Atom toAtom = l.getFirst();
                    if (filter.isVisible(toAtom)) {
                        printEdge(p, fromAtom, toAtom);
                    }
                    l = l.getRest();
                }
            }
        }
    }

    private void printEdge(final PrintStream p, final Atom fromAtom, final Atom toAtom) {
        p.print(fromAtom.getId());
        p.print('\t');
        p.print(toAtom.getId());
        p.print('\n');
    }
}
