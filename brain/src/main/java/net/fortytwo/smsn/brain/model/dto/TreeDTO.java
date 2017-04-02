package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.model.entities.KeyValueTree;

public class TreeDTO<K, V> implements KeyValueTree<K, V> {

    private K key;
    private V value;
    private EntityList<KeyValueTree<K, V>> children;

    @Override
    public void destroy() {
        // nothing to do
    }

    @Override
    public K getKey() {
        return key;
    }

    @Override
    public boolean setKey(K key) {
        this.key = key;
        return false;
    }

    @Override
    public V getValue() {
        return value;
    }

    @Override
    public boolean setValue(V value) {
        this.value = value;
        return false;
    }

    @Override
    public EntityList<KeyValueTree<K, V>> getChildren() {
        return children;
    }

    @Override
    public boolean setChildren(EntityList<KeyValueTree<K, V>> children) {
        this.children = children;
        return false;
    }
}
