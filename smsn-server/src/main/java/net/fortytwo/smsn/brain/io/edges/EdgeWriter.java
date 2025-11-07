package net.fortytwo.smsn.brain.io.edges;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.repository.AtomRepository;

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
        AtomRepository repository = context.getAtomRepository();
        PrintStream p = new PrintStream(context.getDestStream());
        Filter filter = context.getFilter();

        p.println("from\tto");

        for (AtomId fromId : repository.getAllAtomIds()) {
            Atom from = repository.load(fromId);
            if (null != from && repository.testFilter(from, filter)) {
                for (AtomId toId : from.children) {
                    Atom to = repository.load(toId);
                    if (repository.testFilter(to, filter)) {
                        printEdge(p, from.id, to.id);
                    }
                }
            }
        }
    }

    private void printEdge(final PrintStream p, final AtomId fromId, final AtomId toId) {
        p.print(fromId.value);
        p.print('\t');
        p.print(toId.value);
        p.print('\n');
    }
}
