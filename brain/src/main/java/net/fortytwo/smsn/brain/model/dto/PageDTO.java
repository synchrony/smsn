package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.entities.EntityTree;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;

public class PageDTO implements Page {

    private String text;
    private String source;
    private String alias;
    private String shortcut;
    private Float weight;
    private Float priority;

    private EntityTree<Link> content;

    @Override
    public void destroy() {
        // nothing to do
    }

    @Override
    public String getText() {
        return text;
    }

    @Override
    public void setText(String text) {
        this.text = text;
    }

    @Override
    public String getAlias() {
        return alias;
    }

    @Override
    public void setAlias(String alias) {
        this.alias = alias;
    }

    @Override
    public String getSource() {
        return source;
    }

    @Override
    public void setSource(String source) {
        this.source = source;
    }

    @Override
    public String getShortcut() {
        return shortcut;
    }

    @Override
    public void setShortcut(String shortcut) {
        this.shortcut = shortcut;
    }

    @Override
    public EntityTree<Link> getContent() {
        return content;
    }

    @Override
    public void setContent(EntityTree<Link> tree) {
        content = tree;
    }

    @Override
    public Float getWeight() {
        return weight;
    }

    @Override
    public void setWeight(Float weight) {
        this.weight = weight;
    }

    @Override
    public Float getPriority() {
        return priority;
    }

    @Override
    public void setPriority(Float priority) {
        this.priority = priority;
    }
}
