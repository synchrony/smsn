package net.fortytwo.smsn.brain.model.pg;

import com.google.common.base.Function;
import com.google.common.collect.Iterators;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;

import java.util.Iterator;

public class TinkerGraphWrapper extends GraphWrapper {
    public TinkerGraphWrapper(TinkerGraph graph) {
        super(graph);
    }

    @Override
    protected IndexWrapper createFullTextIndex(String key) {
        return new TinkerIndexWrapper(key);
    }

    @Override
    protected IndexWrapper createExactIndex(String key, boolean caseSensitive) {
        if (!hasVertexIndex(key)) {
            createVertexIndex(key);
        }
        
        return new TinkerIndexWrapper(key);
    }

    @Override
    protected void updateIndex(Vertex updatedVertex, String key, Object value) {
        // do nothing
    }

    @Override
    public void begin() {
        // do nothing
    }

    @Override
    public void commit() {
        // do nothing
    }

    @Override
    public void rollback() {
        // do nothing
    }

    @Override
    public void shutdown() {
        // do nothing
    }

    private boolean hasVertexIndex(final String key) {
        return ((TinkerGraph) graph).getIndexedKeys(Vertex.class).contains(key);
    }

    private void createVertexIndex(final String key) {
        ((TinkerGraph) graph).createIndex(key, Vertex.class);
    }

    private class TinkerIndexWrapper extends IndexWrapper {

        public TinkerIndexWrapper(String key) {
            super(key);
        }

        @Override
        public Iterator<Sortable<Vertex, Float>> get(String value) {
            Iterator<Vertex> simple = graph.traversal().V().has(key, value);
            return Iterators.transform(simple, vertex -> new Sortable<>(vertex, 1f));
        }

        @Override
        public void add(Vertex vertex, String key, Object value) {
            // do nothing
        }

        @Override
        public void remove(Vertex vertex, String key) {
            // do nothing
        }
    }
}
