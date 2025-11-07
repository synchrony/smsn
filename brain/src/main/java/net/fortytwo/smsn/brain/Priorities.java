package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.repository.AtomRepository;

import java.util.Comparator;
import java.util.PriorityQueue;
import java.util.Queue;

/**
 * A dynamically updated list of atoms ordered by their priority value
 */
public class Priorities {
    private final PriorityQueue<Atom> queue;
    private final AtomRepository repository;

    public Priorities(AtomRepository repository) {
        this.repository = repository;
        this.queue = new PriorityQueue<>(1, new AtomPriorityComparator());
    }

    public Queue<Atom> getQueue() {
        return queue;
    }

    public void refreshQueue() {
        queue.clear();

        new Thread(() -> {
            SemanticSynchrony.getLogger().info("generating priority queue");
            long startTime = System.currentTimeMillis();

            for (AtomId atomId : repository.getAllAtomIds()) {
                Atom atom = repository.load(atomId);
                if (atom.priority.isPresent()) {
                    updatePriority(atom);
                }
            }

            long endTime = System.currentTimeMillis();
            SemanticSynchrony.getLogger().info("\tfinished generating priority queue in " + (endTime - startTime) + "ms");
        }).start();
    }

    public void updatePriority(final Atom atom) {
        queue.remove(atom);
        if (atom.priority.isPresent()) {
            queue.add(atom);
        }
    }

    public void updatePriorityById(final AtomId atomId) {
        Atom atom = repository.load(atomId);
        updatePriority(atom);
    }

    // order primarily by descending priority, secondarily by descending weight
    private static class AtomPriorityComparator implements Comparator<Atom> {
        public int compare(final Atom a, final Atom b) {
            Float pa = a.priority.isPresent() ? a.priority.get().value : null;
            Float pb = b.priority.isPresent() ? b.priority.get().value : null;

            if (null == pa) {
                return null == pb || 0 == pb ? 0 : -1;
            } else if (null == pb) {
                return 1;
            } else {
                int c = pa.compareTo(pb);
                return 0 == c
                        ? Float.compare(b.weight.value, a.weight.value)
                        : c;
            }
        }
    }
}
