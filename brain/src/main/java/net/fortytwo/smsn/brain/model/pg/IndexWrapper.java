package net.fortytwo.smsn.brain.model.pg;

import org.apache.tinkerpop.gremlin.structure.Vertex;

import java.util.Iterator;

abstract class IndexWrapper {
    protected final String key;

    protected IndexWrapper(String key) {
        this.key = key;
    }

    public abstract Iterator<Sortable<Vertex, Float>> get(final String value);

    public abstract void add(final Vertex vertex, final String key, final Object value);

    public abstract void remove(final Vertex vertex, final String key);
}
