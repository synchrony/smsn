package net.fortytwo.myotherbrain;

import net.fortytwo.myotherbrain.access.AccessManager;
import net.fortytwo.myotherbrain.access.Session;
import net.fortytwo.myotherbrain.model.MOBModel;
import net.fortytwo.myotherbrain.model.MOBModelConnection;
import net.fortytwo.myotherbrain.model.concepts.Association;
import net.fortytwo.myotherbrain.model.concepts.Atom;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;
import net.fortytwo.myotherbrain.update.actions.BreakAssociation;
import net.fortytwo.myotherbrain.update.actions.SetName;
import org.openrdf.sail.Sail;
import org.openrdf.sail.memory.MemoryStore;

/**
 * User: josh
 * Date: 6/18/11
 * Time: 8:20 PM
 */
public class MOBPlay {


    private void doit() throws Exception {
        MOBStore store = MOBStore.getDefaultStore();

        /*
        Repository repo = new SailRepository(store.getSail());
        RepositoryConnection rc = repo.getConnection();
        try {
            rc.add(new File("/Users/josh/projects/fortytwo/myotherbrain/backup/dump_2009_07_16.trig"), "", RDFFormat.TRIG);
            rc.commit();
        } finally {
            rc.close();
        } */

        store.dump(System.out);
    }

    public static void main(final String[] args) throws Exception {
        (new MOBPlay()).doit();

        //*
        java.util.logging.Logger logger = java.util.logging.Logger.getLogger(MyOtherBrain.class.getName());
        logger.info(MyOtherBrain.getVersionInfo());

        Sail sail = new MemoryStore();
        sail.initialize();
        try {
            MOBStore store = new MOBStore(sail);
            store.initialize();

            try {
                store.generateSeedData();

                AccessManager am = new AccessManager(store);
                am.createAccount("bob", "bobspassword", "bob@example.org");
                am.changeUserName("bob", "robert");
                am.changeUserName("robert", "bobby");

                Session session = am.createSession("bobby");
                MOBModel model = session.getModel();
                MOBModelConnection c = model.createConnection();
                try {
                    WriteContext wc = new WriteContext(c);

                    Atom telephone = wc.createAtom(Atom.class);
                    telephone.setName("telephone");
                    telephone.setDescription("a device for voice communication at a distance");
                    Atom red = wc.createAtom(Atom.class);
                    red.setName("red");
                    red.setDescription("the color red");
                    Association a = wc.createAtom(Association.class);
                    a.setSubject(telephone);
                    a.setObject(red);

                    WriteAction action = new SetName(MyOtherBrain.toURI(red.getQName()), "blue", wc);
                    action.redo(wc);
                    action.undo(wc);

                    WriteAction action2 = new BreakAssociation(MyOtherBrain.toURI(a.getQName()), wc);
                    action2.redo(wc);
                    action2.undo(wc);

                    c.commit();
                } finally {
                    c.close();
                }
            } finally {
                store.dump(System.out);
                store.shutDown();
            }
        } finally {
            sail.shutDown();
        }         //*/
    }
}
