package net.fortytwo.myotherbrain.flashmob;

import net.fortytwo.myotherbrain.MOBStore;
import net.fortytwo.myotherbrain.MyOtherBrain;
import net.fortytwo.myotherbrain.access.AccessManager;
import net.fortytwo.myotherbrain.access.Session;
import net.fortytwo.myotherbrain.access.error.NoSuchAccountException;
import net.fortytwo.myotherbrain.flashmob.actions.ActionBean;
import net.fortytwo.myotherbrain.flashmob.model.FirstClassItemBean;
import net.fortytwo.myotherbrain.flashmob.model.FreetextSearchResult;
import net.fortytwo.myotherbrain.flashmob.model.SessionInfo;
import net.fortytwo.myotherbrain.model.MOB;
import net.fortytwo.myotherbrain.model.MOBModelConnection;
import net.fortytwo.myotherbrain.tools.properties.PropertyException;
import net.fortytwo.myotherbrain.tools.properties.TypedProperties;
import net.fortytwo.myotherbrain.update.UpdateException;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;
import org.apache.log4j.Logger;

import java.util.List;

/**
 * Author: josh
 * Date: Jul 10, 2009
 * Time: 7:56:48 PM
 */
public class FlashMOBSession {
    private static final Logger LOGGER = MyOtherBrain.getLogger(FlashMOBSession.class);

    public enum SensitivityLevel {
        PUBLIC(MOB.PUBLIC, 0),
        PERSONAL(MOB.PERSONAL, 1),
        PRIVATE(MOB.PRIVATE, 2);

        private String uri;
        private int value;

        private SensitivityLevel(final String uri,
                                 final int value) {
            this.uri = uri;
            this.value = value;
        }

        public boolean exceeds(final SensitivityLevel other) {
            return this.value > other.value;
        }

        public String toString() {
            return uri;
        }

        public static SensitivityLevel fromURI(final String uri) {
            for (SensitivityLevel l : values()) {
                if (l.uri.equals(uri)) {
                    return l;
                }
            }
            
            return null;
        }
    }

    public static final int
            URIMINTING_MAXBATCHSIZE;

    private static final String
            TEMP_USERNAME = "josh",
            TEMP_PASSWORD = "letmein",
            TEMP_CONTACTEMAILADDRESS = "josh@fortytwo.net";

    static {
        TypedProperties props = MyOtherBrain.getProperties();
        String prefix = "net.fortytwo.myotherbrain.";
        try {
            URIMINTING_MAXBATCHSIZE = props.getInt(prefix + "uriMintingMaxBatchSize");
        } catch (PropertyException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private final Session session;

    private SensitivityLevel currentVisibilityLevel = SensitivityLevel.fromURI(MOB.PERSONAL);

    public static void main(final String[] args) throws Exception {
        MOBStore store = MOBStore.getDefaultStore();

        /*
        FlashMOBSession f = new FlashMOBSession();
        MOBModelConnection c = f.createConnection();
        try {
            String baseURI = "http://example.org/ns/resource";
            QName qName1 = new QName(baseURI + "1");
            QName qName2 = new QName(baseURI + "2");
            ExperimentalClassConcept e1, e2;


            //e1 = c.getElmoManager().create(qName1, ExperimentalClassConcept.class);
            //e2 = c.getElmoManager().create(qName2, ExperimentalClassConcept.class);
            //c.getElmoManager().remove(e1);
            //c.getElmoManager().remove(e2);

            e1 = new ExperimentalClassConcept();
            e2 = new ExperimentalClassConcept();
            e1.setQName(new QName(baseURI + "1"));
            e2.setQName(new QName(baseURI + "2"));


            e1.setName(null);
            e2.setName("2nd");
            e1.setNext(e2);
            e2.setNext(e1);
            c.getElmoManager().persist(e1);

            c.commit();
        } finally {
            c.close();
        }*/

        //store.generateSeedData();
        //AccessManager am = new AccessManager(store);
        //am.createAccount(TEMP_USERNAME, TEMP_PASSWORD, TEMP_CONTACTEMAILADDRESS);
        store.dump(System.out);
    }

    public FlashMOBSession() throws MOBStore.MOBStoreException, NoSuchAccountException {
        System.out.println("FlashMOBSession constructor called");
        LOGGER.info("FlashMOBSession constructor called");

        AccessManager am = new AccessManager(MOBStore.getDefaultStore());
        session = am.createSession(TEMP_USERNAME);
    }

    ////////////////////////////////////

    public SessionInfo getSessionInfo() {
        SessionInfo info = new SessionInfo();
        info.setBaseURI(MyOtherBrain.MOB_RESOURCE_NS + "r");
        info.setUserName(session.getUserName());
        info.setVersionInfo(MyOtherBrain.getVersionInfo());
        info.setVisibilityLevel(currentVisibilityLevel.toString());
        return info;
    }

    public SessionInfo setVisibilityLevel(final String visibilityLevel) {
        SensitivityLevel l = SensitivityLevel.fromURI(visibilityLevel);
        if (null != l) {
            this.currentVisibilityLevel = l;
        } else {
            throw new IllegalArgumentException("not a valid sensitivity level: " + visibilityLevel);
        }

        return getSessionInfo();
    }

    public FreetextSearchResult evaluateFreetextQuery(final String query) {
        // TODO
        return null;
    }

    public List<FirstClassItemBean> getItems() {
        MOBModelConnection c = createConnection();
        try {
            return FlashMOBQuery.getAllFirstClassItems(c);
        } finally {
            c.close();
        }
    }

    public List<FirstClassItemBean> getItemsAssociatedFrom(final FirstClassItemBean it) {
        MOBModelConnection c = createConnection();
        try {
            return FlashMOBQuery.getItemsAssociatedFrom(it, c);
        } finally {
            c.close();
        }
    }

    /*
    public ExperimentalClassConcept getExperimentalObject() {
        String uri = "http://example.org/ns/experResource";

        MOBModelConnection c = createConnection();
        try {
            ExperimentalClassConcept e
                    = new ExperimentalClassConcept();
            //        = c.getElmoManager().create(new QName(uri), ExperimentalClassConcept.class);
            e.setQName(new QName(uri));
            //e.setName("a brand new resource");
            e.setDescription("this is a description, not a name");
            c.getElmoManager().persist(e);
            c.commit();
            //e = new ExperimentalClassConcept(e);
            return e;
        } finally {
            c.close();
        }
    }

    public String[] mintRandomURIs(final int batchSize) {
        if (0 > batchSize) {
            throw new IllegalArgumentException("negative batch size");
        } else if (batchSize > URIMINTING_MAXBATCHSIZE) {
            throw new IllegalArgumentException(
                    "requested batch is too large (maximum size is " + URIMINTING_MAXBATCHSIZE + ")");
        }

        String[] results = new String[batchSize];
        for (int i = 0; i < batchSize; i++) {
            results[i] = MyOtherBrain.randomURIString();
        }

        return results;
    }*/

    public void enqueueAction(final ActionBean bean) throws UpdateException {
        System.out.println("enqueueing action: " + bean);
        WriteContext c = createWriteContext();
        try {
            WriteAction a = StaticStuff.createAction(bean, c);
            a.redo(c);
            c.getConnection().commit();
            System.out.println("...done");
        } finally {
            c.close();
        }
    }

    /*public Association getAssociationExperimental() {
        MOBModelConnection c = createConnection();
        try {

        } finally {
            c.close();
        }
    }*/

    ////////////////////////////////////

    private MOBModelConnection createConnection() {
        return session.getModel().createConnection();
    }

    private WriteContext createWriteContext() {
        return new WriteContext(createConnection());
    }
}
