package net.fortytwo.extendo.brain;

import com.tinkerpop.blueprints.Vertex;
import net.fortytwo.extendo.Extendo;

import java.util.Comparator;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.logging.Logger;

/**
 * A dynamically updated list of atoms ordered by their priority value
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Priorities {
    private static final Logger LOGGER = Extendo.getLogger(Priorities.class);

    private final PriorityQueue<Atom> queue;

    public Priorities() {
        queue = new PriorityQueue<Atom>(1, new AtomPriorityComparator());
    }

    public Queue<Atom> getQueue() {
        return queue;
    }

    public void refreshQueue(final BrainGraph graph) {
        queue.clear();

        new Thread(new Runnable() {
            public void run() {
                LOGGER.info("generating priority queue");
                long startTime = System.currentTimeMillis();

                for (Vertex v : graph.getGraph().getVertices()) {
                    if (null != v.getProperty(Extendo.PRIORITY)) {
                        Atom a = graph.getAtom(v);
                        updatePriority(a);
                    }
                }

                long endTime = System.currentTimeMillis();
                LOGGER.info("\tfinished generating priority queue in " + (endTime - startTime) + "ms");
            }
        }).start();
    }

    public void updatePriority(final Atom a) {
        queue.remove(a);
        if (null != a.getPriority()) {
            queue.add(a);
        }
    }

    // order primarily by descending priority, secondarily by descending weight
    private static class AtomPriorityComparator implements Comparator<Atom> {
        public int compare(final Atom a, final Atom b) {
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
