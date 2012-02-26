package net.fortytwo.myotherbrain.notes;

import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.MOBGraph;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NotesHistory {
    private static final int CAPACITY = 1000;

    private final String[] visitedAtoms;
    private int totalVisits;

    public NotesHistory() {
        this.visitedAtoms = new String[CAPACITY];
        totalVisits = 0;
    }

    public void visit(final String atomId) {
        visitedAtoms[totalVisits % CAPACITY] = atomId;
        totalVisits++;
    }

    public List<String> getHistory(final int maxlen,
                                   final boolean dedup,
                                   final MOBGraph graph,
                                   final Filter filter) {
        Collection<String> r = dedup
                ? new LinkedHashSet<String>()
                : new LinkedList<String>();

        int low = Math.max(totalVisits - CAPACITY, 0);

        for (int i = totalVisits - 1; i >= low; i--) {
            if (r.size() >= maxlen) {
                break;
            }

            String id = visitedAtoms[i % CAPACITY];

            Atom a = graph.getAtom(id);
            if (null != a && filter.isVisible(a)) {
                r.add(id);
            }
        }

        if (dedup) {
            List<String> s = new LinkedList<String>();
            s.addAll(r);
            return s;
        } else {
            return (List<String>) r;
        }
    }
}
