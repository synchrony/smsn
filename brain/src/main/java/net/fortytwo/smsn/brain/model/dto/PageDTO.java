package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.model.entities.KeyValueTree;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;

public class PageDTO implements Page {

    private String format;
    private String text;
    private Float weight;
    private String source;
    private Float priority;
    private Long created;
    private String alias;
    private String shortcut;
    private KeyValueTree<Link, EntityList<Link>> topicTree;

    @Override
    public void destroy() {
        // nothing to do
    }

    @Override
    public String getFormat() {
        return format;
    }

    @Override
    public boolean setFormat(String format) {
        this.format = format;
        return false;
    }

    @Override
    public String getText() {
        return text;
    }

    @Override
    public boolean setText(String text) {
        this.text = text;
        return false;
    }

    @Override
    public String getAlias() {
        return alias;
    }

    @Override
    public boolean setAlias(String alias) {
        this.alias = alias;
        return false;
    }

    @Override
    public Long getCreated() {
        return created;
    }

    @Override
    public boolean setCreated(Long created) {
        this.created = created;
        return false;
    }

    @Override
    public Float getPriority() {
        return priority;
    }

    @Override
    public boolean setPriority(Float priority) {
        this.priority = priority;
        return false;
    }

    @Override
    public String getSource() {
        return source;
    }

    @Override
    public boolean setSource(String source) {
        this.source = source;
        return false;
    }

    @Override
    public String getShortcut() {
        return shortcut;
    }

    @Override
    public boolean setShortcut(String shortcut) {
        this.shortcut = shortcut;
        return false;
    }

    @Override
    public Float getWeight() {
        return weight;
    }

    @Override
    public boolean setWeight(Float weight) {
        this.weight = weight;
        return false;
    }

    @Override
    public Topic getPrimaryTopic() {
        return null == topicTree ? null : topicTree.getKey().getTarget();
    }

    @Override
    public boolean setPrimaryTopic(Topic topic) {
        throw new IllegalArgumentException();
    }

    @Override
    public KeyValueTree<Link, EntityList<Link>> getContent() {
        return topicTree;
    }

    @Override
    public boolean setContent(KeyValueTree<Link, EntityList<Link>> tree) {
        topicTree = tree;
        return false;
    }
}
