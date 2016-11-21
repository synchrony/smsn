package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;

import java.util.Iterator;

public abstract class GraphWrapper {
    protected final Graph graph;

    protected GraphWrapper(Graph graph) {
        this.graph = graph;

        // TODO: add id strategy
        createKeyIndex(SemanticSynchrony.ID_V);
        createKeyIndex(SemanticSynchrony.SHORTCUT);

        createFullTextIndex(SemanticSynchrony.VALUE);
        createFullTextIndex(SemanticSynchrony.ACRONYM);
        //createExactIndex(SemanticSynchrony.SHORTCUT, false);
    }

    protected abstract void createFullTextIndex(String key);

    protected abstract void createExactIndex(String key, boolean caseSensitive);

    protected abstract void createKeyIndex(String key);

    protected abstract void updateIndex(Vertex updatedVertex, String key, Object value);

    public abstract boolean isTransactional();

    public abstract void begin();

    public abstract void commit();

    public abstract void rollback();

    public abstract void shutdown();

    public void reindex(final Vertex vertex) {
        updateIndex(vertex, SemanticSynchrony.ID_V);
        updateIndex(vertex, SemanticSynchrony.VALUE);
        updateIndex(vertex, SemanticSynchrony.ACRONYM);
    }

    private void updateIndex(final Vertex vertex,
                             final String key) {
        VertexProperty property = vertex.property(key);
        Object value = property.isPresent() ? property.value() : null;

        updateIndex(vertex, key, value);
    }

    public Graph getGraph() {
        return graph;
    }

    protected abstract Iterator<Vertex> queryByKeyValue(String key, String value);

    public Iterator<Vertex> queryById(final String id) {
        return graph.traversal().V().has(SemanticSynchrony.ID_V, id);
    }

    public Iterator<Vertex> queryByValue(final String term) {
        return queryByKeyValue(SemanticSynchrony.VALUE, term);
    }

    public Iterator<Vertex> queryByAcronym(final String acronym) {
        return queryByKeyValue(SemanticSynchrony.ACRONYM, acronym);
    }

    public Iterator<Vertex> queryByShortcut(final String shortcut) {
        return graph.traversal().V().has(SemanticSynchrony.SHORTCUT, shortcut);
    }
}
