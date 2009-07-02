package net.fortytwo.myotherbrain;

import net.fortytwo.myotherbrain.access.AccountManager;
import net.fortytwo.myotherbrain.access.Session;
import net.fortytwo.myotherbrain.model.beans.Association;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.tools.properties.TypedProperties;
import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.openrdf.sail.Sail;
import org.openrdf.sail.memory.MemoryStore;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Random;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Dec 12, 2008
 * Time: 4:04:20 PM
 * To change this template use File | Settings | File Templates.
 */
public class MyOtherBrain {
    public static final String MOB_ONTOLOGY_FILE = "myotherbrain.owl";
    public static final String DEFAULT_BASEURI = "http://example.org/replaceThisBaseURI#";

    // Configuration properties.
    public static final String
            NATIVESTORE_DIRECTORY = "net.fortytwo.myotherbrain.nativeStoreDirectory",
            NEOSAIL_DIRECTORY = "net.fortytwo.myotherbrain.neoSailDirectory",
            RMISAILCLIENT_URI = "net.fortytwo.myotherbrain.rmiSailClientURI",
            SAIL_TYPE = "net.fortytwo.myotherbrain.sailType";

    /*public static final URI[] LEGAL_DATATYPE_URIS = {
            XMLSchema.STRING
    };*/

    private static final String MOB_PROPERTIES_FILE = "myotherbrain.properties";
    private static final String LOGGING_PROPERTIES_FILE = "log4j.properties";
    private static final TypedProperties PROPERTIES;
    private static final Logger LOGGER;

    static {
        PropertyConfigurator.configure(
                MyOtherBrain.class.getResource(LOGGING_PROPERTIES_FILE));

        LOGGER = getLogger(MyOtherBrain.class);

        PROPERTIES = new TypedProperties();
        try {
            PROPERTIES.load(MyOtherBrain.class.getResourceAsStream(MOB_PROPERTIES_FILE));
        } catch (IOException e) {
            LOGGER.error("unable to load properties file " + MOB_PROPERTIES_FILE);
            System.exit(1);
        }
    }

    public static Logger getLogger(final Class c) {
        return Logger.getLogger(c);
    }

    public static TypedProperties getProperties() {
        return PROPERTIES;
    }

    public static void main(final String[] args) throws Exception {
        java.util.logging.Logger logger = java.util.logging.Logger.getLogger(MyOtherBrain.class.getName());
        logger.info("testing logging.........");
        
        Sail sail = new MemoryStore();
        sail.initialize();
        try {
            MOBStore store = new MOBStore(sail);
            store.initialize();
            try {
                store.generateSeedData();

                AccountManager am = new AccountManager(store);
                am.createAccount("bob", "bobspassword", "bob@example.org");
                am.changeUserName("bob", "robert");
                am.changeUserName("robert", "bobby");

                Session session = am.createSession("bobby");
                MOBModel model = session.getModel();
                MOBModelConnection c = model.createConnection();
                try {
                    FirstClassItem telephone = c.create(FirstClassItem.class);
                    telephone.setName("telephone");
                    telephone.setDescription("a device for voice communication at a distance");
                    FirstClassItem red = c.create(FirstClassItem.class);
                    red.setName("red");
                    red.setDescription("the color red");
                    Association a = c.create(Association.class);
                    a.setSubject(telephone);
                    a.setObject(red);

                    c.commit();
                } finally {
                    c.close();
                }

                store.dump(System.out);
            } finally {
                store.shutDown();
            }
        } finally {
            sail.shutDown();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // temporary stuff /////////////////////////////////////////////////////////

    // TODO: make this configurable
    private static final String MOB_RESOURCE_NS = "http://myotherbrain.fortytwo.net/resource/";
    private static final Random random = new Random();

    // TODO: move me
    public static String randomURIString() {
        // TODO: improve me
        return MOB_RESOURCE_NS + "r" + random.nextInt(100000);
    }

    private static final MessageDigest SHA1_DIGEST;

    static {
        try {
            SHA1_DIGEST = MessageDigest.getInstance("SHA1");
        } catch (NoSuchAlgorithmException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static String sha1SumOf(final String key) {
        SHA1_DIGEST.update(key.getBytes());
        String hash = new String();
        byte[] digest = SHA1_DIGEST.digest();
        for (int i = 0; i < digest.length; i++) {
            String hex = Integer.toHexString(digest[i]);
            if (hex.length() == 1)
                hex = "0" + hex;
            hex = hex.substring(hex.length() - 2);
            hash = hash + hex;
        }
        return hash;
    }
}
