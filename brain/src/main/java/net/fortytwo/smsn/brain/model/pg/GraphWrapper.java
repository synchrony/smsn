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

    protected abstract Vertex getVertexByKeyValue(String key, String value);

    protected abstract Iterator<Vertex> getVerticesByKeyValue(String key, String value);

    public Vertex getVertexById(final String id) {
        return getVertexByKeyValue(SemanticSynchrony.ID_V, id);
    }

    public Iterator<Vertex> getVerticesByValue(final String term) {
        return getVerticesByKeyValue(SemanticSynchrony.VALUE, term);
    }

    public Iterator<Vertex> getVerticesByAcronym(final String acronym) {
        return getVerticesByKeyValue(SemanticSynchrony.ACRONYM, acronym);
    }

    public Iterator<Vertex> getVerticesByShortcut(final String shortcut) {
        return getVerticesByKeyValue(SemanticSynchrony.SHORTCUT, shortcut);
    }
}
