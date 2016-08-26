package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;
import net.fortytwo.smsn.brain.model.pg.TinkerGraphWrapper;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;

public class BrainTestBase {

    protected AtomGraph createTinkerAtomGraph() {
        TinkerGraph g = TinkerGraph.open();
        TinkerGraphWrapper wrapper = new TinkerGraphWrapper(g);
        return new PGAtomGraph(wrapper);
    }
}
