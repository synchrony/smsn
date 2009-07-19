package net.fortytwo.myotherbrain;

import net.fortytwo.myotherbrain.access.AccessManager;
import net.fortytwo.myotherbrain.access.Session;
import net.fortytwo.myotherbrain.model.MOBModel;
import net.fortytwo.myotherbrain.model.MOBModelConnection;
import net.fortytwo.myotherbrain.model.concepts.Association;
import net.fortytwo.myotherbrain.model.concepts.FirstClassItem;
import net.fortytwo.myotherbrain.tools.properties.TypedProperties;
import net.fortytwo.myotherbrain.tools.properties.PropertyException;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;
import net.fortytwo.myotherbrain.update.actions.BreakAssociation;
import net.fortytwo.myotherbrain.update.actions.SetName;
import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.openrdf.sail.Sail;
import org.openrdf.sail.memory.MemoryStore;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
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
            SAIL_CLASS = "net.fortytwo.myotherbrain.sailClass",
            NAME = "net.fortytwo.myotherbrain.name",
            VERSION = "net.fortytwo.myotherbrain.version",
            REVISION = "net.fortytwo.myotherbrain.revision";

    /*public static final URI[] LEGAL_DATATYPE_URIS = {
            XMLSchema.STRING
    };*/

    private static final String CONFIG_PROPERTIES_FILE = "myotherbrain.properties";
    private static final String VERSION_PROPERTIES_FILE = "version.properties";
    private static final String LOGGING_PROPERTIES_FILE = "log4j.properties";
    private static final TypedProperties CONFIG_PROPERTIES;
    private static final TypedProperties VERSION_PROPERTIES;
    private static final Logger LOGGER;

    static {
        PropertyConfigurator.configure(
                MyOtherBrain.class.getResource(LOGGING_PROPERTIES_FILE));

        LOGGER = getLogger(MyOtherBrain.class);

        CONFIG_PROPERTIES = new TypedProperties();
        VERSION_PROPERTIES = new TypedProperties();
        try {
            CONFIG_PROPERTIES.load(MyOtherBrain.class.getResourceAsStream(CONFIG_PROPERTIES_FILE));
            VERSION_PROPERTIES.load(MyOtherBrain.class.getResourceAsStream(VERSION_PROPERTIES_FILE));
        } catch (IOException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static Logger getLogger(final Class c) {
        return Logger.getLogger(c);
    }

    public static TypedProperties getProperties() {
        return CONFIG_PROPERTIES;
    }

    public static String getVersionInfo() {
        String name;
        String version;
        int revision;

        try {
            name = VERSION_PROPERTIES.getString(NAME);
            version = VERSION_PROPERTIES.getString(VERSION);
            revision = VERSION_PROPERTIES.getInt(REVISION);
        } catch (PropertyException e) {
            throw new IllegalStateException(e);
        }

        return name + " " + version + ", revision #" + revision;
    }

    public static void main(final String[] args) throws Exception {
        java.util.logging.Logger logger = java.util.logging.Logger.getLogger(MyOtherBrain.class.getName());
        logger.info(getVersionInfo());

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

                    FirstClassItem telephone = wc.create(FirstClassItem.class);
                    telephone.setName("telephone");
                    telephone.setDescription("a device for voice communication at a distance");
                    FirstClassItem red = wc.create(FirstClassItem.class);
                    red.setName("red");
                    red.setDescription("the color red");
                    Association a = wc.create(Association.class);
                    a.setSubject(telephone);
                    a.setObject(red);

                    WriteAction action = new SetName(toURI(red.getQName()), "blue", wc);
                    action.redo(wc);
                    action.undo(wc);

                    WriteAction action2 = new BreakAssociation(toURI(a.getQName()), wc);
                    action2.redo(wc);
                    action2.undo(wc);

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

    public static URI toURI(final QName q) {
        try {
            return new URI(q.getNamespaceURI() + q.getLocalPart());
        } catch (URISyntaxException e) {
            throw new IllegalArgumentException(e);
        }
    }

    // TODO: make this configurable
    public static final String MOB_RESOURCE_NS = "http://myotherbrain.fortytwo.net/resource/";
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
        String hash = "";
        byte[] digest = SHA1_DIGEST.digest();
        for (byte b : digest) {
            String hex = Integer.toHexString(b);
            if (hex.length() == 1)
                hex = "0" + hex;
            hex = hex.substring(hex.length() - 2);
            hash = hash + hex;
        }
        return hash;
    }
}
