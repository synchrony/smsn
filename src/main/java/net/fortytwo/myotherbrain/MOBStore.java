package net.fortytwo.myotherbrain;

import net.fortytwo.myotherbrain.model.MOB;
import net.fortytwo.myotherbrain.model.MOBModel;
import net.fortytwo.myotherbrain.model.concepts.Account;
import net.fortytwo.myotherbrain.model.concepts.Association;
import net.fortytwo.myotherbrain.model.concepts.FirstClassItem;
import net.fortytwo.myotherbrain.model.concepts.GeoPoint;
import net.fortytwo.myotherbrain.model.concepts.Graph;
import net.fortytwo.myotherbrain.model.concepts.Literal;
import net.fortytwo.myotherbrain.model.concepts.Marker;
import net.fortytwo.myotherbrain.model.concepts.SensitivityLevel;
import net.fortytwo.myotherbrain.model.concepts.WebResource;
import net.fortytwo.myotherbrain.tools.properties.PropertyException;
import net.fortytwo.myotherbrain.tools.properties.TypedProperties;
import org.apache.log4j.Logger;
import org.neo4j.api.core.EmbeddedNeo;
import org.neo4j.api.core.NeoService;
import org.neo4j.rdf.sail.NeoSail;
import org.neo4j.rdf.sail.rmi.RmiSailClient;
import org.neo4j.rdf.store.CachingLuceneIndexService;
import org.neo4j.rdf.store.VerboseQuadStore;
import org.neo4j.rdf.fulltext.FulltextIndex;
import org.neo4j.rdf.fulltext.SimpleFulltextIndex;
import org.neo4j.util.index.IndexService;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.ElmoModule;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFParseException;
import org.openrdf.rio.Rio;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailException;
import org.openrdf.sail.memory.MemoryStore;
import org.openrdf.sail.nativerdf.NativeStore;

import javax.xml.namespace.QName;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URI;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.util.Properties;

/**
 * Author: josh
 * Date: Feb 27, 2009
 * Time: 12:05:43 PM
 */
public class MOBStore {
    private static final Logger LOGGER = MyOtherBrain.getLogger(MOBStore.class);

    private static MOBStore defaultStore;

    private final Sail sail;
    private Repository repository;
    private ElmoModule elmoModule;
    private boolean initialized = false;

    public static MOBStore getDefaultStore() throws MOBStoreException {
        if (null == defaultStore) {
            defaultStore = new MOBStore();
            defaultStore.initialize();

            Runtime.getRuntime().addShutdownHook(new Thread("shutdown hook for default MOB store") {
                @Override
                public void run() {
                    try {
                        defaultStore.shutDown();
                        //defaultStore.getSail().shutDown();
                    } catch (Throwable t) {
                        LOGGER.error("failure in store shutdown", t);
                    }
                }
            });
        }

        return defaultStore;
    }

    public class MOBStoreException extends Exception {
        public MOBStoreException(final Throwable cause) {
            super(cause);
        }

        public MOBStoreException(final String msg) {
            super(msg);
        }
    }

    private MOBStore() throws MOBStoreException {
        Properties props = MyOtherBrain.getProperties();
        String sailType = props.getProperty(MyOtherBrain.SAIL_CLASS);
        sail = createSail(sailType);
    }

    public MOBStore(final Sail sail) {
        this.sail = sail;
    }

    public void initialize() throws MOBStoreException {
        if (initialized) {
            throw new IllegalStateException("store has already been initialized");
        }

        LOGGER.debug("initializing MOB store");

        repository = new SailRepository(sail);

        ElmoModule baseModule = new ElmoModule();
        // This analysis will be query-only in the final module.
        baseModule.setGraph(new QName(MOB.MOBONTOLOGYGRAPH));

        // Dynamically register concepts from the MOB ontology.
        baseModule.addConcept(Thing.class);
        baseModule.addConcept(Account.class);
        baseModule.addConcept(Association.class);
        baseModule.addConcept(FirstClassItem.class);
        baseModule.addConcept(GeoPoint.class);
        baseModule.addConcept(Graph.class);
        baseModule.addConcept(Literal.class);
        baseModule.addConcept(Marker.class);
        baseModule.addConcept(SensitivityLevel.class);
        baseModule.addConcept(WebResource.class);

        baseModule.addConcept(ExperimentalClassConcept.class, MOB.NAMESPACE + "ExperimentalClassConcept");

        elmoModule = new ElmoModule();
        elmoModule.includeModule(baseModule);

        initialized = true;
    }

    public Sail getSail() {
        if (!initialized) {
            throw new IllegalStateException("not yet initialized");
        }

        return sail;
    }

    public Repository getRepository() {
        if (!initialized) {
            throw new IllegalStateException("not yet initialized");
        }

        return repository;
    }

    public ElmoModule getElmoModule() {
        if (!initialized) {
            throw new IllegalStateException("not yet initialized");
        }

        return elmoModule;
    }

    public void shutDown() throws MOBStoreException {
        if (!initialized) {
            throw new IllegalStateException("not yet initialized");
        }

        LOGGER.debug("shutting down MOB store");

        // Note: elmoModule doesn't need to be closed or shutDown.

        try {
            sail.shutDown();
        } catch (SailException e) {
            throw new MOBStoreException(e);
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // convenience methods, may be moved ///////////////////////////////////////

    public MOBModel createModel(final QName writableGraph) {
        return new MOBModel(getSail(), getElmoModule(), getRepository(), writableGraph);
    }

    public void dump(final OutputStream out) throws RepositoryException, RDFHandlerException {
        RDFHandler h = Rio.createWriter(RDFFormat.TRIG, out);
        RepositoryConnection rc = getRepository().getConnection();
        try {
            rc.export(h);
        } finally {
            rc.close();
        }
    }

    public void generateSeedData() throws RepositoryException, IOException, RDFParseException {
        RepositoryConnection rc = getRepository().getConnection();
        try {
            InputStream is = MyOtherBrain.class.getResourceAsStream(
                    MyOtherBrain.MOB_ONTOLOGY_FILE);
            org.openrdf.model.URI ontoGraph = getSail().getValueFactory().createURI(MOB.MOBONTOLOGYGRAPH);
            try {
                rc.add(is, MyOtherBrain.DEFAULT_BASEURI, RDFFormat.RDFXML, ontoGraph);
            } finally {
                is.close();
            }

            rc.commit();
        } finally {
            rc.close();
        }
    }

    ////////////////////////////////////////////////////////////////////////////

    private Sail createSail(final String sailType) throws MOBStoreException {
        System.out.println("creating Sail of type: " + sailType);
        Sail sail;

        if (sailType.equals(MemoryStore.class.getName())) {
            sail = createMemoryStore();
        } else if (sailType.equals(NativeStore.class.getName())) {
            sail = createNativeStore();
        } else if (sailType.equals(NeoSail.class.getName())) {
            sail = createNeoSail();
        } else if (sailType.equals(RmiSailClient.class.getName())) {
            sail = createRMISailClient();
        } else {
            throw new MOBStoreException("unhandled Sail type: " + sailType);
        }

        return sail;
    }

    private Sail createMemoryStore() throws MOBStoreException {
        LOGGER.debug("instantiating MemoryStore");

        Sail sail = new MemoryStore();
        try {
            sail.initialize();
        } catch (SailException e) {
            throw new MOBStoreException(e);
        }

        return sail;
    }

    private Sail createNativeStore() throws MOBStoreException {
        TypedProperties props = MyOtherBrain.getProperties();
        File dir;

        try {
            dir = props.getFile(MyOtherBrain.NATIVESTORE_DIRECTORY);
        } catch (PropertyException e) {
            throw new MOBStoreException(e);
        }

        LOGGER.debug("insstantiating NativeStore in directory: " + dir);
        Sail sail = new NativeStore(dir);
        try {
            sail.initialize();
        } catch (SailException e) {
            throw new MOBStoreException(e);
        }

        return sail;
    }

    private Sail createNeoSail() throws MOBStoreException {
        TypedProperties props = MyOtherBrain.getProperties();

        String dir, fulltextDir;
        try {
            dir = props.getString(MyOtherBrain.NEOSAIL_DIRECTORY);
            fulltextDir = props.getString(MyOtherBrain.NEOFULLTEXT_DIRECTORY, null);
        } catch (PropertyException e) {
            throw new MOBStoreException(e);
        }
        LOGGER.debug("instantiating NeoSail in directory: " + dir);
        final NeoService neo = new EmbeddedNeo(dir);
        neo.enableRemoteShell();

        final IndexService idx = new CachingLuceneIndexService(neo);
        Runtime.getRuntime().addShutdownHook(new Thread("Neo shutdown hook") {
            @Override
            public void run() {
                idx.shutdown();
                neo.shutdown();
            }
        });

        FulltextIndex fulltextIndex = (null == fulltextDir)
                ? null
                : new SimpleFulltextIndex(neo, new File(fulltextDir));

        VerboseQuadStore store = (null == fulltextIndex)
                ? new VerboseQuadStore(neo, idx)
                : new VerboseQuadStore(neo, idx, null, fulltextIndex);
        Sail sail = new NeoSail(neo, store);

        try {
            sail.initialize();
        } catch (SailException e) {
            throw new MOBStoreException(e);
        }

        return sail;
    }

    private Sail createRMISailClient() throws MOBStoreException {
        TypedProperties props = MyOtherBrain.getProperties();
        URI uri;
        try {
            uri = props.getURI(MyOtherBrain.RMISAILCLIENT_URI);
        } catch (PropertyException e) {
            throw new MOBStoreException(e);
        }
        Sail sail;

        try {
            sail = new RmiSailClient(uri);
        } catch (MalformedURLException e) {
            throw new MOBStoreException(e);
        } catch (RemoteException e) {
            throw new MOBStoreException(e);
        } catch (NotBoundException e) {
            throw new MOBStoreException(e);
        }

        // Note: don't initialize this Sail, as it should already have been
        // initialized on the server side.

        return sail;
    }
}
