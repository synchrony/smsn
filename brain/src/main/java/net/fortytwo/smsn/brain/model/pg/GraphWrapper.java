package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.T;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public abstract class GraphWrapper {

    protected final Graph graph;

    private final Map<String, IndexWrapper> indices = new HashMap<>();

    protected GraphWrapper(Graph graph) {
        this.graph = graph;

        // TODO: add id strategy
        add(createExactIndex(SemanticSynchrony.PropertyKeys.ID, true));
        add(createExactIndex(SemanticSynchrony.PropertyKeys.SHORTCUT, true));

        add(createFullTextIndex(SemanticSynchrony.PropertyKeys.TITLE));
        add(createFullTextIndex(SemanticSynchrony.PropertyKeys.ACRONYM));
    }

    protected abstract IndexWrapper createFullTextIndex(String key);

    protected abstract IndexWrapper createExactIndex(String key, boolean caseSensitive);

    protected abstract void updateIndex(Vertex updatedVertex, String key, Object value);

    public abstract void begin();

    public abstract void commit();

    public abstract void rollback();

    public abstract void shutdown();

    public Vertex createVertex(final AtomId id, final String label) {
        Vertex vertex = graph.addVertex(T.label, label);
        // TODO: use id strategy
        vertex.property(SemanticSynchrony.PropertyKeys.ID, getNonNullId(id).value);
        // TODO: use auto-indexing
        updateIndex(vertex, SemanticSynchrony.PropertyKeys.ID);

        return vertex;
    }

    public Graph getGraph() {
        return graph;
    }

    public Vertex getVertexById(final AtomId id) {
        return getVertexByKeyValue(SemanticSynchrony.PropertyKeys.ID, id.value);
    }

    public Iterator<Sortable<Vertex, Float>> getVerticesByTitle(final String term) {
        return getVerticesByKeyValue(SemanticSynchrony.PropertyKeys.TITLE, term);
    }

    public Iterator<Sortable<Vertex, Float>> getVerticesByAcronym(final String acronym) {
        return getVerticesByKeyValue(SemanticSynchrony.PropertyKeys.ACRONYM, acronym);
    }

    public Iterator<Sortable<Vertex, Float>> getVerticesByShortcut(final String shortcut) {
        return getVerticesByKeyValue(SemanticSynchrony.PropertyKeys.SHORTCUT, shortcut);
    }

    protected IndexWrapper getIndex(final String key) {
        return indices.get(key);
    }

    private AtomId getNonNullId(final AtomId id) {
        return null == id ? SemanticSynchrony.createRandomId() : id;
    }

    private void add(final IndexWrapper index) {
        indices.put(index.key, index);
    }

    public void updateIndex(final Vertex vertex,
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
                SemanticSynchrony.getLogger().warning("multiple vertices with " + key + " '" + value + "'");
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
