package net.fortytwo.extendo.rfid;

import com.tinkerpop.blueprints.impls.rexster.RexsterGraph;
import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.BrainGraph;
import net.fortytwo.extendo.brain.Filter;
import net.fortytwo.extendo.brain.NoteQueries;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Collection;
import java.util.LinkedList;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RFIDPlay {
    private static final Logger LOGGER = Logger.getLogger(RFIDPlay.class.getName());

    public RFIDPlay(final String graphUri) throws Exception {

        RexsterGraph g = new RexsterGraph(graphUri);

        BrainGraph graph = new BrainGraph(g);

        RFIDListener l = new RFIDListener();

        RFIDReader reader = new RFIDReader(l.firstReader(), 4, System.out);

        //String readerName = reader.doReaderCommand("get ReaderName");
        //LOGGER.info("reader name: " + readerName);

        Filter f = new Filter();

        BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
        while (true) {
            reader.clear();

            for (int i = 0; i < 10; i++) {
            reader.readTags();
            }

            //System.out.println("[tag list]");
            for (String tagId : reader.getTagIds()) {
                String v = "RFID: " + tagId;
                Collection<Atom> atoms = graph.getAtomsWithValue(v);
                if (0 == atoms.size()) {
                    System.out.println("         * " + v);
                } else {
                    if (atoms.size() > 1) {
                        LOGGER.warning("multiple atoms with value: " + v);
                    }

                    for (Atom a : atoms) {
                        System.out.println("* :" + a.asVertex().getId() + ": " + a.getValue());
                        for (Atom n : getParents(a, f)) {
                            System.out.println("* :" + n.asVertex().getId() + ": " + n.getValue());
                        }
                        System.out.println("");
                    }
                }
            }

            System.out.println("");

            String command = r.readLine();
            if (command.toLowerCase().equals("quit")) {
                break;
            }
        }
    }

    private static Collection<Atom> getParents(final Atom a,
                                               final Filter f) {
        Collection<Atom> n = new LinkedList<Atom>();

        //for (Atom at : NoteQueries.FORWARD_ADJACENCY.getLinked(a, f)) {
        //    n.add(at);
        //}

        for (Atom at : NoteQueries.BACKWARD_ADJACENCY.getLinked(a, f)) {
            n.add(at);
        }

        return n;
    }

    public static void main(final String args[]) {
        try {
            new RFIDPlay("http://localhost:8182/graphs/joshkb");
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
