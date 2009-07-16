package net.fortytwo.myotherbrain.update.actions;

import junit.framework.TestCase;
import net.fortytwo.myotherbrain.model.MOBModel;
import net.fortytwo.myotherbrain.MOBStore;
import net.fortytwo.myotherbrain.access.AccessManager;
import net.fortytwo.myotherbrain.access.Session;
import org.openrdf.sail.Sail;
import org.openrdf.sail.memory.MemoryStore;

/**
 * Author: josh
 * Date: Jul 3, 2009
 * Time: 12:00:01 PM
 */
public abstract class WriteActionTestCase extends TestCase {
    protected Sail sail;
    protected MOBStore store;
    protected MOBModel model;

    public void setUp() throws Exception {
        sail = new MemoryStore();
        sail.initialize();

        store = new MOBStore(sail);
        store.initialize();

        AccessManager manager = new AccessManager(store);
        manager.createAccount("testuser", "testpassword", "testuser@example.org");
        Session session = manager.createSession("testuser");
        model = session.getModel();
    }

    public void tearDown() throws Exception {
        store.shutDown();
        sail.shutDown();
    }
}