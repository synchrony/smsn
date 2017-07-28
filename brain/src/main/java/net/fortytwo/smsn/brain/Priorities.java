package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.TopicGraph;

import java.util.Comparator;
import java.util.PriorityQueue;
import java.util.Queue;

/**
 * A dynamically updated list of atoms ordered by their priority value
 */
public class Priorities {
    private final PriorityQueue<Note> queue;

    public Priorities() {
        queue = new PriorityQueue<>(1, new AtomPriorityComparator());
    }

    public Queue<Note> getQueue() {
        return queue;
    }

    public void refreshQueue(final TopicGraph graph) {
        queue.clear();

        new Thread(() -> {
            SemanticSynchrony.getLogger().info("generating priority queue");
            long startTime = System.currentTimeMillis();

            for (Note a : graph.getAllNotes()) {
                if (null != a.getPriority()) {
                    updatePriority(a);
                }
            }

            long endTime = System.currentTimeMillis();
            SemanticSynchrony.getLogger().info("\tfinished generating priority queue in " + (endTime - startTime) + "ms");
        }).start();
    }

    public void updatePriority(final Note a) {
        queue.remove(a);
        if (null != a.getPriority()) {
            queue.add(a);
        }
    }

    // order primarily by descending priority, secondarily by descending weight
    private static class AtomPriorityComparator implements Comparator<Note> {
        public int compare(final Note a, final Note b) {
            Float pa = a.getPriority();
            Float pb = b.getPriority();

            if (null == pa) {
                return null == pb || 0 == pb ? 0 : -1;
            } else if (null == pb) {
                return 1;
            } else {
                int c = pa.compareTo(pb);
                return 0 == c
                        ? b.getWeight().compareTo(a.getWeight())
                        : c;
            }
        }
    }
}
