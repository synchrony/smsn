package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.repository.AtomRepository;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

public class History {
    private static final int CAPACITY = 1000;

    private final AtomId[] visited;
    private int totalVisits;

    public History() {
        this.visited = new AtomId[CAPACITY];
        totalVisits = 0;
    }

    public void visit(final AtomId noteId) {
        // repeated actions upon the same note count as a single visit
        if (totalVisits == 0 || !noteId.equals(getLastVisit())) {
            appendVisit(noteId);
        }
    }

    /**
     * Get recent visit history as Atoms.
     *
     * @param maxlen the maximum number of atoms to return
     * @param repository the atom repository
     * @param filter filter for atoms
     * @return list of recently visited atoms
     */
    public List<Atom> getHistory(final int maxlen,
                                  final AtomRepository repository,
                                  final Filter filter) {
        List<Atom> atoms = new LinkedList<>();

        int low = Math.max(totalVisits - CAPACITY, 0);

        for (int i = totalVisits - 1; i >= low; i--) {
            if (atoms.size() >= maxlen) {
                break;
            }

            AtomId id = visited[i % CAPACITY];

            // Load atom if it exists and check filter
            // Skip atoms that no longer exist (may have been deleted or from a different session)
            java.util.Optional<Atom> optAtom = repository.findById(id);
            if (optAtom.isPresent()) {
                Atom atom = optAtom.get();
                if (filter == null || repository.testFilter(atom, filter)) {
                    atoms.add(atom);
                }
            }
        }

        return atoms;
    }

    /**
     * Get recent visit history as AtomIds.
     *
     * @param maxlen the maximum number of IDs to return
     * @return list of recently visited atom IDs
     */
    public List<AtomId> getHistoryIds(final int maxlen) {
        List<AtomId> ids = new LinkedList<>();

        int low = Math.max(totalVisits - CAPACITY, 0);

        for (int i = totalVisits - 1; i >= low && ids.size() < maxlen; i--) {
            AtomId id = visited[i % CAPACITY];
            if (id != null) {
                ids.add(id);
            }
        }

        return ids;
    }

    private AtomId getLastVisit() {
        return visited[(totalVisits - 1) % CAPACITY];
    }

    private void appendVisit(final AtomId noteId) {
        visited[totalVisits % CAPACITY] = noteId;
        totalVisits++;
    }
}
