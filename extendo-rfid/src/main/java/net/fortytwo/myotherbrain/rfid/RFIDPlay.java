package net.fortytwo.myotherbrain.rfid;

import com.alien.enterpriseRFID.reader.AlienClass1Reader;
import com.alien.enterpriseRFID.tags.Tag;
import com.tinkerpop.blueprints.impls.rexster.RexsterGraph;
import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.ExtendoGraph;
import net.fortytwo.myotherbrain.ExtendoGraph;
import net.fortytwo.myotherbrain.notes.Filter;
import net.fortytwo.myotherbrain.notes.NoteQueries;

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

    public RFIDPlay() throws Exception {

        RexsterGraph g = new RexsterGraph("http://localhost:8182/graphs/tinkernotes");

        ExtendoGraph graph = ExtendoGraph.getInstance(g);

        RFIDListener l = new RFIDListener();

        AlienClass1Reader reader = l.firstReader();

        LOGGER.info("reader: " + reader);
        reader.setUsername("alien");
        reader.setPassword("password");

        reader.setAntennaSequence("0 1 2 3");
        reader.setTagType(16);

        //String readerName = reader.doReaderCommand("get ReaderName");
        //LOGGER.info("reader name: " + readerName);

        Filter f = new Filter();

        BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
        while (true) {
            // This avoids "broken pipe" errors
            reader.close();
            reader.open();

            Tag[] tagList = reader.getTagList();
            if (null != tagList) {
                //System.out.println("[tag list]");
                for (Tag t : tagList) {
                    String v = "RFID: " + t.getTagID();
                    Collection<Atom> atoms = graph.getAtomsWithValue(v);
                    if (0 == atoms.size()) {
                        System.out.println("         * " + v);
                    } else {
                        if (atoms.size() > 1) {
                            LOGGER.warning("multiple atoms with value: " + v);
                        }

                        for (Atom a : atoms) {
                            System.out.println(a.asVertex().getId() + ": * " + a.getValue());
                            for (Atom n : getNeighbors(a, f)) {
                                System.out.println(n.asVertex().getId() + ":     * " + n.getValue());
                            }
                        }
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

    private static Collection<Atom> getNeighbors(final Atom a,
                                                 final Filter f) {
        Collection<Atom> n = new LinkedList<Atom>();

        for (Atom at : NoteQueries.FORWARD_ADJACENCY.getLinked(a, f)) {
            n.add(at);
        }

        for (Atom at : NoteQueries.BACKWARD_ADJACENCY.getLinked(a, f)) {
            n.add(at);
        }

        return n;
    }

    public static void main(final String args[]) {
        try {
            new RFIDPlay();
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
