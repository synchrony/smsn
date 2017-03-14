package net.fortytwo.smsn.brain.model.entities;

public interface KeyValueTree<K, V> extends Entity {

    K getKey();

    boolean setKey(K key);

    V getValue();

    boolean setValue(V value);

    EntityList<KeyValueTree<K, V>> getChildren();

    boolean setChildren(EntityList<KeyValueTree<K, V>> children);
}
