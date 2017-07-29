package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Topic;

import java.util.Collection;
import java.util.function.Consumer;

public class NoteDTO implements Note {

    private String id = SemanticSynchrony.createRandomId();
    private Topic topic;
    private Role role;
    private long created = System.currentTimeMillis();
    private String title = "atom " + id;
    private Float weight = SemanticSynchrony.DEFAULT_WEIGHT;
    private Float priority = SemanticSynchrony.DEFAULT_PRIORITY;
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

    public Role getRole() {
        return role;
    }

    public void setRole(final Role role) {
        this.role = role;
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
    public Topic getTopic() {
        return topic;
    }

    @Override
    public void setTopic(final Topic topic) {
        this.topic = topic;
    }

    @Override
    public ListNode<Note> getChildren() {
        return null;
    }

    @Override
    public void setChildren(ListNode<Note> notes) {
    }

    @Override
    public void forFirstOf(Consumer<ListNode<Note>> consumer) {
    }

    @Override
    public void addChildAt(Note child, int position) {
    }

    @Override
    public void deleteChildAt(int position) {
    }

    @Override
    public Collection<ListNode<Note>> getFirstOf() {
        return null;
    }

    @Override
    public Note getSubject(ListNode<Note> notes) {
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
