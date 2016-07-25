package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.Filter;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteHistory {
    private static final int CAPACITY = 1000;

    private final String[] visitedAtoms;
    private int totalVisits;

    public NoteHistory() {
        this.visitedAtoms = new String[CAPACITY];
        totalVisits = 0;
    }

    public void visit(final String atomId) {
        visitedAtoms[totalVisits % CAPACITY] = atomId;
        totalVisits++;
    }

    public List<String> getHistory(final int maxlen,
                                   final boolean dedup,
                                   final AtomGraph graph,
                                   final Filter filter) {
        Collection<String> r = dedup
                ? new LinkedHashSet<>()
                : new LinkedList<>();

        int low = Math.max(totalVisits - CAPACITY, 0);

        for (int i = totalVisits - 1; i >= low; i--) {
            if (r.size() >= maxlen) {
                break;
            }

            String id = visitedAtoms[i % CAPACITY];

            Atom v = graph.getAtom(id);
            if (null != v && filter.isVisible(v)) {
                r.add(id);
            }
        }

        if (dedup) {
            List<String> s = new LinkedList<>();
            s.addAll(r);
            return s;
        } else {
            return (List<String>) r;
        }
    }
}
