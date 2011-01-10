package net.fortytwo.myotherbrain.flashmob;

import net.fortytwo.myotherbrain.MOBStore;
import net.fortytwo.myotherbrain.MyOtherBrain;
import net.fortytwo.myotherbrain.access.AccessManager;
import net.fortytwo.myotherbrain.access.Session;
import net.fortytwo.myotherbrain.flashmob.query.FlashMOBQuery;
import net.fortytwo.myotherbrain.flashmob.query.FlashMOBQueryExecutor;
import net.fortytwo.myotherbrain.flashmob.query.FlashMOBQueryResult;
import net.fortytwo.myotherbrain.flashmob.update.ActionFactory;
import net.fortytwo.myotherbrain.flashmob.update.actions.ActionBean;
import net.fortytwo.myotherbrain.model.MOBOntology;
import net.fortytwo.myotherbrain.model.MOBModelConnection;
import net.fortytwo.myotherbrain.query.QueryException;
import net.fortytwo.myotherbrain.tools.properties.PropertyException;
import net.fortytwo.myotherbrain.tools.properties.TypedProperties;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;
import org.apache.log4j.Logger;

import javax.xml.namespace.QName;
import java.util.List;

/**
 * Author: josh
 * Date: Jul 10, 2009
 * Time: 7:56:48 PM
 */
public class FlashMOBSession {
    private static final Logger LOGGER = MyOtherBrain.getLogger(FlashMOBSession.class);

    public enum SensitivityLevel {
        PUBLIC(MOBOntology.PUBLIC, 0),
        PERSONAL(MOBOntology.PERSONAL, 1),
        PRIVATE(MOBOntology.PRIVATE, 2);

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

            throw new IllegalArgumentException("no such sensitivity level: " + uri);
        }

        public static SensitivityLevel find(final net.fortytwo.myotherbrain.model.concepts.SensitivityLevel l) {
            QName q = l.getQName();
            return fromURI(q.getNamespaceURI() + q.getLocalPart());
        }
    }

    public static final int
            URIMINTING_MAXBATCHSIZE;

    private static final String
            TEMP_USERNAME = "josh",
            TEMP_PASSWORD = "letmein",
            TEMP_CONTACTEMAILADDRESS = "josh@fortytwo.net";

    static {
        TypedProperties props = MyOtherBrain.getConfiguration();
        String prefix = "net.fortytwo.myotherbrain.";
        try {
            URIMINTING_MAXBATCHSIZE = props.getInt(prefix + "uriMintingMaxBatchSize");
        } catch (PropertyException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private final Session session;
    private final FlashMOBQueryExecutor queryExecutor;

    // Mutable values which control visibility of items and relationships.
    private SensitivityLevel sensitivityUpperBound = SensitivityLevel.fromURI(MOBOntology.PERSONAL);
    private float emphasisLowerBound = 0f;

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

    public FlashMOBSession() throws Throwable {
        System.out.println("FlashMOBSession constructor called");
        LOGGER.info("FlashMOBSession constructor called");

        try {
            AccessManager am = new AccessManager(MOBStore.getDefaultStore());
            session = am.createSession(TEMP_USERNAME);
            queryExecutor = new FlashMOBQueryExecutor(session.getModel(), this.getSessionInfo());
        } catch (Throwable t) {
            t.printStackTrace();
            throw t;
        }
    }

    ////////////////////////////////////

    public SessionInfo getSessionInfo() {
        SessionInfo info = new SessionInfo();
        info.setUserName(session.getUserName());
        info.setVersionInfo(MyOtherBrain.getVersionInfo());
        info.setSensitivityUpperBound(sensitivityUpperBound.toString());
        info.setEmphasisLowerBound(emphasisLowerBound);
        return info;
    }

    public SessionInfo setSensitivityUpperBound(final String maxSensitivity) {
        SensitivityLevel l = SensitivityLevel.fromURI(maxSensitivity);
        if (null != l) {
            this.sensitivityUpperBound = l;
        } else {
            throw new IllegalArgumentException("not a valid sensitivity level: " + maxSensitivity);
        }

        return getSessionInfo();
    }

    public SessionInfo setEmphasisLowerBound(final float minEmphasis) {
        if (minEmphasis < 0) {
            throw new IllegalArgumentException("threshold must be greater than or equal to zero");
        } else if (minEmphasis > 1) {
            throw new IllegalArgumentException("threshold must be less than or equal to one");
        }

        emphasisLowerBound = minEmphasis;
        return getSessionInfo();
    }

    public List<FlashMOBQueryResult> executeQuery(final FlashMOBQuery query,
                                                  final int startIndex,
                                                  final int endIndex) throws QueryException {
        return queryExecutor.execute(query, startIndex, endIndex);
    }

    // FIXME: use the undo/redo queue
    public void doActions(final List<ActionBean> actions) throws Throwable {
        for (ActionBean bean : actions) {
            System.out.println("enqueueing action: " + bean);
            WriteContext c = createWriteContext();
            try {
                try {
                    WriteAction a = ActionFactory.createAction(bean, c);
                    a.redo(c);
                    c.getConnection().commit();
                    System.out.println("...done");
                } catch (Throwable t) {
                    t.printStackTrace();
                    throw t;
                }
            } finally {
                c.close();
            }
        }
    }

    public void undoActions(final List<ActionBean> actions) {
        // TODO
    }

    public void redoActions(final List<ActionBean> actions) {
        // TODO
    }

    ////////////////////////////////////

    private MOBModelConnection createConnection() {
        return session.getModel().createConnection();
    }

    private WriteContext createWriteContext() {
        return new WriteContext(createConnection());
    }
}
