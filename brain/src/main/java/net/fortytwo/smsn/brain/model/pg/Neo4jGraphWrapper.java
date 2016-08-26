package net.fortytwo.smsn.brain.model.pg;

import com.google.common.collect.Iterators;
import net.fortytwo.smsn.SemanticSynchrony;
import org.apache.tinkerpop.gremlin.neo4j.structure.Neo4jGraph;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Label;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseFactory;
import org.neo4j.graphdb.factory.GraphDatabaseSettings;
import org.neo4j.graphdb.index.Index;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.graphdb.schema.IndexDefinition;
import org.neo4j.graphdb.schema.Schema;
import org.neo4j.helpers.collection.MapUtil;
import org.neo4j.tinkerpop.api.impl.Neo4jGraphAPIImpl;

import java.io.File;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Neo4jGraphWrapper extends GraphWrapper {
    private static final Logger logger = SemanticSynchrony.getLogger(Neo4jGraphWrapper.class);

    private static String indexNameForKey(final String key) {
        return key + "-index";
    }

    public Neo4jGraphWrapper(File dataDir) {
        super(createGraph(dataDir));
    }

    @Override
    protected void updateIndex(Vertex updatedVertex, String key, Object value) {
        Node node = getGraphDatabaseService().getNodeById((Long) updatedVertex.id());
        if (null == node) throw new IllegalArgumentException();

        Index<Node> index = getIndex(key);
        if (null == value) {
            index.remove(node, key);
        } else {
            index.add(node, key, value);
        }
    }

    @Override
    protected void createFullTextIndex(final String key) {
        createIndex(key, IndexManager.PROVIDER, "lucene", "type", "fulltext");
    }

    @Override
    protected void createExactIndex(final String key, boolean caseSensitive) {
        if (caseSensitive) {
            createIndex(key, "type", "exact");
        } else {
            createIndex(key, "type", "exact", "to_lower_case", "true");
        }
    }

    @Override
    protected void createKeyIndex(String key) {
        if (keyIndexExists(key)) return;

        GraphDatabaseService graphDb = getGraphDatabaseService();
        try (Transaction tx = graphDb.beginTx()) {
            Schema schema = graphDb.schema();
            schema.indexFor(Label.label(SemanticSynchrony.ATOM))
                    .on(key)
                    .create();
            tx.success();
        }
    }

    private boolean keyIndexExists(final String key) {
        GraphDatabaseService graphDb = getGraphDatabaseService();
        try (Transaction tx = graphDb.beginTx()) {
            Schema schema = graphDb.schema();
            for (IndexDefinition definition : schema.getIndexes(Label.label(SemanticSynchrony.ATOM))) {
                for (String existingKey : definition.getPropertyKeys()) {
                    if (existingKey.equals(key)) return true;
                }
            }
        }

        return false;
    }

    @Override
    protected Iterator<Vertex> queryByKeyValue(String key, String value) {
        System.out.println("query (" + key + "," + value + ")");

        Index<Node> index = getIndex(key);
        IndexHits<Node> hits = index.query(key, value);

        return Iterators.transform(hits, this::nodeToVertex);
    }

    @Override
    public void shutdown() {
        try {
            getNeo4jGraph().close();
        } catch (Exception e) {
            logger.log(Level.WARNING, "failed to shut down Neo4j graph properly", e);
        }
    }

    private void createIndex(final String key, final String... keysAndValues) {
        String indexName = indexNameForKey(key);
        GraphDatabaseService graphDb = getGraphDatabaseService();
        IndexManager indexManager = graphDb.index();
        try (Transaction tx = graphDb.beginTx()) {
            if (!indexManager.existsForNodes(indexName)) {
                indexManager.forNodes(indexName,
                        MapUtil.stringMap(keysAndValues));

                tx.success();
                logger.info("created Neo4j index '" + indexName + "'");
            }
        }
    }

    private Vertex nodeToVertex(final Node node) {
        return getNeo4jGraph().vertices(node.getId()).next();
    }

    private Neo4jGraph getNeo4jGraph() {
        return (Neo4jGraph) graph;
    }

    private GraphDatabaseService getGraphDatabaseService() {
        // TODO: it would be nice if TinkerPop would expose the GraphDatabaseService
        return ((Neo4jGraphAPIImpl) getNeo4jGraph().getBaseGraph()).getGraphDatabase();
    }

    private static Graph createGraph(final File dataDir) {
        if (!dataDir.exists()) {
            createGraphDatabaseService(dataDir);
        }

        return Neo4jGraph.open(dataDir.getAbsolutePath());
    }

    private Index<Node> getIndex(final String key) {
        String indexName = indexNameForKey(key);
        return getGraphDatabaseService().index().forNodes(indexName);
    }

    private static void createGraphDatabaseService(File dataDir) {
        GraphDatabaseService graphDb = new GraphDatabaseFactory()
                .newEmbeddedDatabaseBuilder(dataDir)
                .setConfig(GraphDatabaseSettings.node_keys_indexable,
                        SemanticSynchrony.VALUE +
                                "," + SemanticSynchrony.ACRONYM +
                                "," + SemanticSynchrony.SHORTCUT)
                .setConfig(GraphDatabaseSettings.node_auto_indexing, "true").
                        newGraphDatabase();

        graphDb.shutdown();
    }
}
