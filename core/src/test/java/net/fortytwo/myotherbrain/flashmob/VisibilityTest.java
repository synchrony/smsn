package net.fortytwo.myotherbrain.flashmob;

import junit.framework.TestCase;

/**
 * Author: josh
 * Date: Jul 25, 2009
 * Time: 10:20:24 PM
 */
public class VisibilityTest extends TestCase {
    public void testNothing() {

    }

    /*
 private Sail sail;
 private MOBStore store;
 private Session session;
 private MOBModel model;
 private FlashMOBSession flashMOBSession;

 private MOBModelConnection c;
 private WriteContext wc;

 public void setUp() throws Exception {
     sail = new MemoryStore();
     sail.initialize();
     store = new MOBStore(sail);
     store.initialize();
     store.generateSeedData();

     AccessManager am = new AccessManager(store);
     am.createAccount("bob", "bobspassword", "bob@example.org");

     session = am.createSession("bob");
     model = session.getModel();

     c = model.createConnection();
     wc = new WriteContext(c);
 }

 public void tearDown() throws Exception {
     c.commit();
     c.close();

     //store.dump(System.out);
     store.shutDown();
     sail.shutDown();
 }

 public void testSensitivityUpperBound() throws Exception {
     FirstClassItem bob = wc.create(FirstClassItem.class);

 }   */
}
