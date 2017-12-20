package net.fortytwo.smsn.brain.model.pg;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.model.Property;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;

public abstract class PGNote extends PGListNode<Note> implements Note {

    private static final Map<String, Consumer<PGNote>> setterTriggersByPropertyKey;

    static {
        setterTriggersByPropertyKey = new HashMap<>();

        setterTriggersByPropertyKey.put(SemanticSynchrony.PropertyKeys.ID, PGNote::idUpdated);
        setterTriggersByPropertyKey.put(SemanticSynchrony.PropertyKeys.LABEL, PGNote::titleUpdated);
        setterTriggersByPropertyKey.put(SemanticSynchrony.PropertyKeys.SHORTCUT, PGNote::shortcutUpdated);
    }

    public PGNote(final Vertex vertex, Function<Vertex, Note> toNote) {
        super(vertex, toNote, SemanticSynchrony.VertexLabels.NOTE);
    }

    @Override
    public Topic getTopic() {
        return getExactlyOneEntity(SemanticSynchrony.EdgeLabels.TOPIC, Direction.OUT, v -> getGraph().asTopic(v));
    }

    @Override
    public void setTopic(final Topic topic) {
        setRequiredEntity(SemanticSynchrony.EdgeLabels.TOPIC, topic);
    }

    @Override
    public Note getParent() {
        return null;
    }

    @Override
    public void addChild(int index, Note child) {
        assertHasNoRest(child);

        setFirst((Note) ListNode.add(this, index, child, (note, rest) -> {
            note.setRest(rest);
            return note;
        }));
    }

    @Override
    public void removeChild(int index) {
        ListNode<Note> removed = ListNode.remove(this, index);
        removed.setRest(null);
        removed.destroy();
    }

    @Override
    public int getNumberOfChildren() {
        return ListNode.lengthOf(getFirst());
    }

    @Override
    public int getNumberOfParents() {
        return null == getParent() ? 0 : 1;
    }

    @Override
    public Role getRole() {
        return getProperty(SemanticSynchrony.PropertyKeys.ROLE);
    }

    @Override
    public void setRole(Role role) {
        setProperty(SemanticSynchrony.PropertyKeys.ROLE, role);
    }

    @Override
    public String getAlias() {
        return getProperty(SemanticSynchrony.PropertyKeys.ALIAS);
    }

    @Override
    public void setAlias(String alias) {
        setProperty(SemanticSynchrony.PropertyKeys.ALIAS, alias);
    }

    @Override
    public Long getCreated() {
        return getProperty(SemanticSynchrony.PropertyKeys.CREATED);
    }

    @Override
    public void setCreated(Long created) {
        setProperty(SemanticSynchrony.PropertyKeys.CREATED, created);
    }

    @Override
    public String getLabel() {
        return getProperty(SemanticSynchrony.PropertyKeys.LABEL);
    }

    @Override
    public void setLabel(String title) {
        setProperty(SemanticSynchrony.PropertyKeys.LABEL, title);
    }

    @Override
    public String getText() {
        return getProperty(SemanticSynchrony.PropertyKeys.TEXT);
    }

    @Override
    public void setText(String text) {
        setProperty(SemanticSynchrony.PropertyKeys.TEXT, text);
    }

    @Override
    public Float getPriority() {
        return getProperty(SemanticSynchrony.PropertyKeys.PRIORITY);
    }

    @Override
    public void setPriority(Float priority) {
        setProperty(SemanticSynchrony.PropertyKeys.PRIORITY, priority);
    }

    @Override
    public String getShortcut() {
        return getProperty(SemanticSynchrony.PropertyKeys.SHORTCUT);
    }

    @Override
    public void setShortcut(String shortcut) {
        setProperty(SemanticSynchrony.PropertyKeys.SHORTCUT, shortcut);
    }

    @Override
    public Float getWeight() {
        return getProperty(SemanticSynchrony.PropertyKeys.WEIGHT);
    }

    @Override
    public void setWeight(Float weight) {
        setProperty(SemanticSynchrony.PropertyKeys.WEIGHT, weight);
    }

    @Override
    public String getSource() {
        return getProperty(SemanticSynchrony.PropertyKeys.SOURCE);
    }

    @Override
    public void setSource(String source) {
        setProperty(SemanticSynchrony.PropertyKeys.SOURCE, source);
    }

    private void updateAcronym() {
        Vertex vertex = asVertex();
        String value = this.getLabel();
        String acronym = valueToAcronym(value);

        VertexProperty<String> previousProperty = vertex.property(SemanticSynchrony.PropertyKeys.ACRONYM);
        if (null != previousProperty) {
            previousProperty.remove();
        }

        if (null != acronym) {
            vertex.property(SemanticSynchrony.PropertyKeys.ACRONYM, acronym);
            getGraph().updateIndex(this, SemanticSynchrony.PropertyKeys.ACRONYM);
        }
    }

    private String valueToAcronym(final String value) {
        // index only short, name-like values, avoiding free-form text if possible
        if (null != value && value.length() <= 100) {
            String clean = cleanForAcronym(value);
            StringBuilder acronym = new StringBuilder();
            boolean isInside = false;
            for (byte b : clean.getBytes()) {
                // TODO: support international letter characters as such
                if (b >= 'a' && b <= 'z') {
                    if (!isInside) {
                        acronym.append((char) b);
                        isInside = true;
                    }
                } else if (' ' == b) {
                    isInside = false;
                }
            }

            return acronym.toString();
        } else {
            return null;
        }
    }

    private String cleanForAcronym(final String value) {
        return value.toLowerCase().replaceAll("[-_\t\n\r]", " ").trim();
    }

    private void idUpdated() {
        getGraph().updateIndex(this, SemanticSynchrony.PropertyKeys.ID);
    }

    private void titleUpdated() {
        getGraph().updateIndex(this, SemanticSynchrony.PropertyKeys.LABEL);
        updateAcronym();
    }

    private void shortcutUpdated() {
        getGraph().updateIndex(this, SemanticSynchrony.PropertyKeys.SHORTCUT);
    }

    private <V> Property<Note, V> getPropertyForKey(final String key) {
        Property<Note, V> property = (Property<Note, V>) Note.propertiesByKey.get(key);
        if (null == property) {
            throw new IllegalArgumentException("no such property: " + key);
        }
        return property;
    }

    public <V> V getProperty(final String key) {
        return getProperty(getPropertyForKey(key));
    }

    public <V> void setProperty(final String key, final V value) {
        setProperty(getPropertyForKey(key), value);
    }

    public <V> V getProperty(final Property<Note, V> property) {
        V value = getOptionalProperty(property.getKey());
        if (null == value) {
            if (null != property.getDefaultValue()) {
                return property.getDefaultValue();
            } else if (property.isRequired()) {
                throw new InvalidGraphException("missing property '" + property.getKey() + "' for " + this);
            } else {
                return null;
            }
        } else {
            return value;
        }
    }

    public <V> void setProperty(final Property<Note, V> property, final V value) {
        V internalValue;
        if (null == value) {
            if (property.isRequired()) {
                throw new IllegalArgumentException("cannot set required property " + property.getKey() + " to null");
            } else {
                internalValue = null;
            }
        } else if (null != property.getDefaultValue() && property.getDefaultValue().equals(value)) {
            internalValue = null;
        } else {
            internalValue = value;
        }

        setOptionalProperty(property.getKey(), internalValue);
        Consumer<PGNote> trigger = setterTriggersByPropertyKey.get(property.getKey());
        if (null != trigger) {
            trigger.accept(this);
        }
    }

    @Override
    public String toString() {
        String id;
        try {
            id = getTopic().getId();
        } catch (IllegalArgumentException e) {
            Function<PGEntity, String> toString = PGEntity::toString;
            return toString.apply(this);
        }
        return "Note[" + id + "]";
    }

    private void assertHasNoRest(final Note note) {
        Preconditions.checkArgument(null == note.getRest());
    }
}
