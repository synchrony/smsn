package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Logger;

public abstract class GraphWrapper {
    private static final Logger logger = SemanticSynchrony.getLogger(GraphWrapper.class);

    protected final Graph graph;

    private final Map<String, IndexWrapper> indices = new HashMap<>();

    protected GraphWrapper(Graph graph) {
        this.graph = graph;

        // TODO: add id strategy
        add(createExactIndex(SemanticSynchrony.ID_V, true));
        add(createExactIndex(SemanticSynchrony.SHORTCUT, true));

        add(createFullTextIndex(SemanticSynchrony.VALUE));
        add(createFullTextIndex(SemanticSynchrony.ACRONYM));
    }

    protected abstract IndexWrapper createFullTextIndex(String key);

    protected abstract IndexWrapper createExactIndex(String key, boolean caseSensitive);

    protected abstract void updateIndex(Vertex updatedVertex, String key, Object value);

    public abstract void begin();

    public abstract void commit();

    public abstract void rollback();

    public abstract void shutdown();

    public void reindex(final Vertex vertex) {
        updateIndex(vertex, SemanticSynchrony.ID_V);
        updateIndex(vertex, SemanticSynchrony.VALUE);
        updateIndex(vertex, SemanticSynchrony.ACRONYM);
        updateIndex(vertex, SemanticSynchrony.SHORTCUT);
    }

    public Graph getGraph() {
        return graph;
    }

    public Vertex getVertexById(final String id) {
        return getVertexByKeyValue(SemanticSynchrony.ID_V, id);
    }

    public Iterator<Sortable<Vertex, Float>> getVerticesByValue(final String term) {
        return getVerticesByKeyValue(SemanticSynchrony.VALUE, term);
    }

    public Iterator<Sortable<Vertex, Float>> getVerticesByAcronym(final String acronym) {
        return getVerticesByKeyValue(SemanticSynchrony.ACRONYM, acronym);
    }

    public Iterator<Sortable<Vertex, Float>> getVerticesByShortcut(final String shortcut) {
        return getVerticesByKeyValue(SemanticSynchrony.SHORTCUT, shortcut);
    }

    protected IndexWrapper getIndex(final String key) {
        return indices.get(key);
    }

    private void add(final IndexWrapper index) {
        indices.put(index.key, index);
    }

    private void updateIndex(final Vertex vertex,
                             final String key) {
        VertexProperty property = vertex.property(key);
        Object value = property.isPresent() ? property.value() : null;

        updateIndex(vertex, key, value);
    }

    private Iterator<Sortable<Vertex, Float>> getFromIndex(final String key, final String value) {
        IndexWrapper index = getIndex(key);
        if (null == index) throw new IllegalStateException();
        return index.get(value);
    }

    private Vertex getVertexByKeyValue(String key, String value) {
        Iterator<Sortable<Vertex, Float>> vertices = getFromIndex(key, value);
        if (vertices.hasNext()) {
            Vertex next = vertices.next().getEntity();
            if (vertices.hasNext()) {
                logger.warning("multiple atoms with " + key + " '" + value + "'");
            }

            return next;
        } else {
            return null;
        }
    }

    private Iterator<Sortable<Vertex, Float>> getVerticesByKeyValue(String key, String value) {
        return getFromIndex(key, value);
    }
}
