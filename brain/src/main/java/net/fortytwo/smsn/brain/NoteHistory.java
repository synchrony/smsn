package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.Filter;

import java.util.Collection;
import java.util.LinkedList;

public class NoteHistory {
    private static final int CAPACITY = 1000;

    private final String[] visitedAtoms;
    private int totalVisits;

    public NoteHistory() {
        this.visitedAtoms = new String[CAPACITY];
        totalVisits = 0;
    }

    public void visit(final String atomId) {
        // repeated actions upon the same atom count as a single visit
        if (totalVisits == 0 || !atomId.equals(getLastVisit())) {
            appendVisit(atomId);
        }
    }

    public Iterable<Atom> getHistory(final int maxlen,
                                     final AtomGraph graph,
                                     final Filter filter) {
        Collection<Atom> atoms = new LinkedList<>();

        int low = Math.max(totalVisits - CAPACITY, 0);

        for (int i = totalVisits - 1; i >= low; i--) {
            if (atoms.size() >= maxlen) {
                break;
            }

            String id = visitedAtoms[i % CAPACITY];

            Atom a = graph.getAtomById(id);
            if (null != a && filter.isVisible(a)) {
                atoms.add(a);
            }
        }

        return atoms;
    }

    private String getLastVisit() {
        return visitedAtoms[(totalVisits - 1) % CAPACITY];
    }

    private void appendVisit(final String atomId) {
        visitedAtoms[totalVisits % CAPACITY] = atomId;
        totalVisits++;
    }
}
