package net.fortytwo.smsn.brain.model.pg;

import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;

import java.util.Iterator;

public class TinkerGraphWrapper extends GraphWrapper {
    public TinkerGraphWrapper(TinkerGraph graph) {
        super(graph);
    }

    @Override
    protected void createFullTextIndex(String key) {
        // unsupported
    }

    @Override
    protected void createExactIndex(String key, boolean caseSensitive) {
        if (!hasVertexIndex(key)) {
            createVertexIndex(key);
        }
    }

    @Override
    protected void createKeyIndex(String key) {
        // do nothing
    }

    @Override
    protected void updateIndex(Vertex updatedVertex, String key, Object value) {
        // do nothing
    }

    @Override
    public void shutdown() {
        // do nothing
    }

    @Override
    protected Iterator<Vertex> queryByKeyValue(String key, String value) {
        throw new UnsupportedOperationException();
    }

    private boolean hasVertexIndex(final String key) {
        return ((TinkerGraph) graph).getIndexedKeys(Vertex.class).contains(key);
    }

    private void createVertexIndex(final String key) {
        ((TinkerGraph) graph).createIndex(key, Vertex.class);
    }
}
