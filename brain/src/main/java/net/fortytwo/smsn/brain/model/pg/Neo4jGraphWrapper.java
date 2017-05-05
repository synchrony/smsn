package net.fortytwo.smsn.brain.model.pg;

import com.google.common.collect.Iterators;
import net.fortytwo.smsn.SemanticSynchrony;
import org.apache.tinkerpop.gremlin.neo4j.structure.Neo4jGraph;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseFactory;
import org.neo4j.graphdb.factory.GraphDatabaseSettings;
import org.neo4j.graphdb.index.Index;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.helpers.collection.MapUtil;
import org.neo4j.tinkerpop.api.impl.Neo4jGraphAPIImpl;

import java.io.File;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Neo4jGraphWrapper extends GraphWrapper {

    private static String indexNameForKey(final String key) {
        return key + "-index";
    }

    public Neo4jGraphWrapper(final Neo4jGraph graph) {
        super(graph);
    }

    public Neo4jGraphWrapper(File dataDir) {
        super(createGraph(dataDir));
    }

    @Override
    protected void updateIndex(Vertex updatedVertex, String key, Object value) {
        IndexWrapper index = getIndex(key);
        if (null == value) {
            index.remove(updatedVertex, key);
        } else {
            index.add(updatedVertex, key, value);
        }
    }

    @Override
    public void begin() {
        graph.tx().readWrite();
    }

    @Override
    public void commit() {
        graph.tx().commit();
    }

    @Override
    public void rollback() {
        graph.tx().rollback();
    }

    @Override
    protected IndexWrapper createFullTextIndex(final String key) {
        Index<Node> index = createIndex(key, IndexManager.PROVIDER, "lucene", "type", "fulltext");
        return new FulltextIndexWrapper(key, index);
    }

    @Override
    protected IndexWrapper createExactIndex(final String key, boolean caseSensitive) {
        Index<Node> index;
        if (caseSensitive) {
            index = createIndex(key, "type", "exact");
        } else {
            index = createIndex(key, "type", "exact", "to_lower_case", "true");
        }

        return new ExactIndexWrapper(key, index);
    }

    @Override
    public void shutdown() {
        try {
            getNeo4jGraph().close();
        } catch (Exception e) {
            SemanticSynchrony.getLogger().log(Level.WARNING, "failed to shut down Neo4j graph properly", e);
        }
    }

    private Index<Node> createIndex(final String key, final String... keysAndValues) {
        String indexName = indexNameForKey(key);
        GraphDatabaseService graphDb = getGraphDatabaseService();
        IndexManager indexManager = graphDb.index();
        try (Transaction tx = graphDb.beginTx()) {
            boolean success = false;
            try {
                Index<Node> index;
                if (!indexManager.existsForNodes(indexName)) {
                    index = indexManager.forNodes(indexName,
                            MapUtil.stringMap(keysAndValues));

                    SemanticSynchrony.getLogger().fine("created Neo4j index '" + indexName + "'");
                } else {
                    index = indexManager.forNodes(indexName);
                }
                success = true;
                return index;
            } finally {
                if (success) {
                    tx.success();
                } else {
                    tx.failure();
                }
            }
        }
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
            createAndShutdownGraphDatabaseService(dataDir);
        }

        return Neo4jGraph.open(dataDir.getAbsolutePath());
    }

    private static void createAndShutdownGraphDatabaseService(File dataDir) {
        GraphDatabaseService graphDb = new GraphDatabaseFactory()
                .newEmbeddedDatabaseBuilder(dataDir)
                .setConfig(GraphDatabaseSettings.node_keys_indexable,
                        SemanticSynchrony.PropertyKeys.TITLE +
                                "," + SemanticSynchrony.PropertyKeys.ACRONYM +
                                "," + SemanticSynchrony.PropertyKeys.SHORTCUT)
                .setConfig(GraphDatabaseSettings.node_auto_indexing, "true").
                        newGraphDatabase();

        graphDb.shutdown();
    }

    private abstract class Neo4jIndexWrapper extends IndexWrapper {
        protected final Index<Node> index;

        protected Neo4jIndexWrapper(String key, Index<Node> index) {
            super(key);
            this.index = index;
        }

        protected Iterator<Sortable<Vertex, Float>> toVertexIterator(final IndexHits<Node> hits) {
            return Iterators.transform(hits, node -> new Sortable<>(nodeToVertex(node), hits.currentScore()));
        }

        private Vertex nodeToVertex(final Node node) {
            return getNeo4jGraph().vertices(node.getId()).next();
        }

        private Node asNode(final Vertex vertex) {
            Node node = getGraphDatabaseService().getNodeById((Long) vertex.id());
            if (null == node) throw new IllegalArgumentException();
            return node;
        }

        @Override
        public void add(final Vertex vertex, final String key, final Object value) {
            index.add(asNode(vertex), key, value);
        }

        @Override
        public void remove(final Vertex vertex, final String key) {
            index.remove(asNode(vertex), key);
        }
    }

    private class ExactIndexWrapper extends Neo4jIndexWrapper {
        public ExactIndexWrapper(String key, Index<Node> index) {
            super(key, index);
        }

        @Override
        public Iterator<Sortable<Vertex, Float>> get(final String value) {
            return toVertexIterator(index.get(key, value));
        }
    }

    private class FulltextIndexWrapper extends Neo4jIndexWrapper {
        public FulltextIndexWrapper(String key, Index<Node> index) {
            super(key, index);
        }

        @Override
        public Iterator<Sortable<Vertex, Float>> get(final String value) {
            return toVertexIterator(index.query(key, value));
        }
    }
}
