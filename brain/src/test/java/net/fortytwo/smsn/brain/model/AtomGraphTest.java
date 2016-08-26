package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.pg.Neo4jGraphWrapper;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;
import org.junit.After;
import org.junit.Before;

import java.io.File;
import java.util.Collection;

public abstract class AtomGraphTest extends BrainTestBase {
    protected Neo4jGraphWrapper graphWrapper;
    protected AtomGraph atomGraph;
    protected Filter filter = new Filter();
    protected Collection<Atom> result;

    @Before
    public void setUp() throws Exception {
        //File dir = File.createTempFile("smsn", "test");
        File dir = new File("/tmp/neo");
        dir.delete();
        //dir.mkdir();
        //dir.deleteOnExit();

        graphWrapper = new Neo4jGraphWrapper(dir);

        atomGraph = new PGAtomGraph(graphWrapper);
    }

    @After
    public void tearDown() throws Exception {
        if (null != graphWrapper) {
            graphWrapper.shutdown();
        }
    }
}
