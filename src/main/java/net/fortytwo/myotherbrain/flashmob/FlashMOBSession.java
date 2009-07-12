package net.fortytwo.myotherbrain.flashmob;

import net.fortytwo.myotherbrain.MOBStore;
import net.fortytwo.myotherbrain.MyOtherBrain;
import net.fortytwo.myotherbrain.access.AccessManager;
import net.fortytwo.myotherbrain.access.Session;
import net.fortytwo.myotherbrain.access.error.NoSuchAccountException;
import net.fortytwo.myotherbrain.flashmob.actions.ActionBean;
import net.fortytwo.myotherbrain.model.MOBModelConnection;
import net.fortytwo.myotherbrain.tools.properties.PropertyException;
import net.fortytwo.myotherbrain.tools.properties.TypedProperties;
import net.fortytwo.myotherbrain.writeapi.WriteAction;
import net.fortytwo.myotherbrain.writeapi.WriteContext;
import net.fortytwo.myotherbrain.writeapi.WriteException;
import org.apache.log4j.Logger;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;
import java.util.List;

/**
 * Author: josh
 * Date: Jul 10, 2009
 * Time: 7:56:48 PM
 */
public class FlashMOBSession {
    private static final Logger LOGGER = MyOtherBrain.getLogger(FlashMOBSession.class);

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

    public static void main(final String[] args) throws Exception {
        MOBStore store = MOBStore.getDefaultStore();
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

    public String getVersionInfo() {
        System.out.println("getVersionInfo has been called.");
        return MyOtherBrain.getVersionInfo();
    }

    ////////////////////////////////////

    public String arbitraryString() {
        return "foobar";
    }

    public Date arbitraryDate() {
        return new Date();
    }

    public Float arbitraryFloat() {
        return 3.1415f;
    }

    public URI arbitraryURI() throws URISyntaxException {
        return new URI("http://example.org/myResource");
    }

    public ExamplePOJO arbitraryExamplePOJO() {
        ExamplePOJO p = new ExamplePOJO();
        p.setName("hummingbird");
        p.setDescription("a small bird with a high metabolism");
        return p;
    }

    ////////////////////////////////////

    public String passString(final String s) {
        return s;
    }

    public Date passDate(final Date d) {
        return d;
    }

    public Float passFloat(final Float f) {
        return f;
    }

    public URI passURI(final URI u) {
        return u;
    }

    public ExamplePOJO passExamplePOJO(final ExamplePOJO p) {
        System.out.println("passing example POJO: " + p.getName() + ", " + p.getDescription());
        return p;
    }

    ////////////////////////////////////

    public List<Item> getItems() {
        MOBModelConnection c = createConnection();
        try {
            return Queries.getAllFirstClassItems(c);
        } finally {
            c.close();
        }
    }

    ////////////////////////////////////

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
    }

    public void enqueueAction(final ActionBean bean) throws WriteException {
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

    ////////////////////////////////////

    private MOBModelConnection createConnection() {
        return session.getModel().createConnection();
    }

    private WriteContext createWriteContext() {
        return new WriteContext(createConnection());
    }
}
