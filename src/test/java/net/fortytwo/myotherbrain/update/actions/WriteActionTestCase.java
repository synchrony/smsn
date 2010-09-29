package net.fortytwo.myotherbrain.update.actions;

import junit.framework.TestCase;
import net.fortytwo.myotherbrain.model.MOBModel;
import net.fortytwo.myotherbrain.MOBStore;
import net.fortytwo.myotherbrain.update.WriteContext;
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
    protected final Sail sail;
    protected final MOBStore store;
    protected final MOBModel model;

    protected WriteContext context;
    
    public WriteActionTestCase() {
        try {
            sail = new MemoryStore();
            sail.initialize();

            store = new MOBStore(sail);
            store.initialize();

            AccessManager manager = new AccessManager(store);
            manager.createAccount("testuser", "testpassword", "testuser@example.org");
            Session session = manager.createSession("testuser");
            model = session.getModel();
        } catch (Throwable t) {
            throw new IllegalStateException(t);
        }

        Runtime.getRuntime().addShutdownHook(new Thread() {
            public void run() {
                //System.out.println("shutting down");
                try {
                    store.shutDown();
                    sail.shutDown();
                } catch (Throwable t) {
                    t.printStackTrace(System.err);
                    System.exit(1);
                }
            }
        });
    }

    public void setUp() throws Exception {
         context = new WriteContext(model.createConnection());
    }

    public void tearDown() throws Exception {
        context.getConnection().rollback();
        context.getConnection().close();
    }
}