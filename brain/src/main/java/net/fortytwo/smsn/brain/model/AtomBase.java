package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.EntityList;

import java.util.Collection;
import java.util.function.Consumer;

public class AtomBase implements Atom {

    private String id = SemanticSynchrony.createRandomId();
    private long created = System.currentTimeMillis();
    private String title = "atom " + id;
    private Float weight = SemanticSynchrony.Weight.DEFAULT;
    private Float priority = SemanticSynchrony.Priority.NONE;
    // TODO: don't hard-code the default source
    private String source = "public";

    @Override
    public void destroy() {
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public void setId(String id) {
        this.id = id;
    }

    @Override
    public String getAlias() {
        return null;
    }

    @Override
    public void setAlias(String alias) {
    }

    @Override
    public Long getCreated() {
        return created;
    }

    @Override
    public void setCreated(Long created) {
        this.created = created;
    }

    @Override
    public String getTitle() {
        return title;
    }

    @Override
    public void setTitle(String title) {
        this.title = title;
    }

    @Override
    public String getText() {
        return null;
    }

    @Override
    public void setText(String text) {
    }

    @Override
    public Float getPriority() {
        return priority;
    }

    @Override
    public void setPriority(Float priority) {
        this.priority = priority;
    }

    @Override
    public String getShortcut() {
        return null;
    }

    @Override
    public void setShortcut(String shortcut) {
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
    public EntityList<Atom> getChildren() {
        return null;
    }

    @Override
    public void setChildren(EntityList<Atom> notes) {
    }

    @Override
    public void forFirstOf(Consumer<EntityList<Atom>> consumer) {
    }

    @Override
    public void addChildAt(Atom child, int position) {
    }

    @Override
    public void deleteChildAt(int position) {
    }

    @Override
    public Collection<EntityList<Atom>> getFirstOf() {
        return null;
    }

    @Override
    public Atom getSubject(EntityList<Atom> notes) {
        return null;
    }

    @Override
    public String getSource() {
        return source;
    }

    @Override
    public void setSource(String source) {
        this.source = source;
    }
}
