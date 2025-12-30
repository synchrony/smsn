package net.fortytwo.smsn.brain.io.freeplane;

import net.fortytwo.smsn.brain.AtomId;

import java.util.ArrayList;
import java.util.List;

/**
 * Mutable intermediate representation of a tree during Freeplane import.
 * Used as a temporary structure before persisting to Atoms.
 */
class TreeData {
    private AtomId id;
    private String title;
    private String text;
    private long created;
    private final List<TreeData> children = new ArrayList<>();

    public AtomId getId() {
        return id;
    }

    public void setId(AtomId id) {
        this.id = id;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public long getCreated() {
        return created;
    }

    public void setCreated(long created) {
        this.created = created;
    }

    public List<TreeData> getChildren() {
        return children;
    }

    public void addChild(TreeData child) {
        children.add(child);
    }

    public int countChildren() {
        return children.size();
    }
}
