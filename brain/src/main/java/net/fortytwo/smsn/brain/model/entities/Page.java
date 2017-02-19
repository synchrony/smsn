package net.fortytwo.smsn.brain.model.entities;

public interface Page extends Entity {

    String getFormat();

    boolean setFormat(String format);

    String getText();

    boolean setText(String text);

    String getAlias();

    boolean setAlias(String alias);

    Long getCreated();

    boolean setCreated(Long created);

    Float getPriority();

    boolean setPriority(Float priority);

    Float getSharability();

    boolean setSharability(Float sharability);

    String getShortcut();

    boolean setShortcut(String shortcut);

    Float getWeight();

    boolean setWeight(Float weight);

    Topic getPrimaryTopic();

    boolean setPrimaryTopic(Topic topic);

    KeyValueTree<Link, EntityList<Link>> getTopicTree();

    boolean setTopicTree(KeyValueTree<Link, EntityList<Link>> tree);
}
