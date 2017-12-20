package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.error.InvalidUpdateException;
import net.fortytwo.smsn.brain.model.Property;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import javax.validation.constraints.NotNull;

/**
 * A service for setting the properties of a note
 */
public class SetProperties extends FilteredAction {

    @NotNull
    private String id;
    @NotNull
    private String name;
    @NotNull
    private Object value;

    private String getId() {
        return notNull(id);
    }

    private String getName() {
        return notNull(name);
    }

    private Object getValue() {
        return notNull(value);
    }

    public void setId(String id) {
        this.id = id;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    private <V> V setProperty(final String key, final Object rawValue) {
        Property<Note, V> prop = (Property<Note, V>) Note.propertiesByKey.get(key);
        if (null == prop) {
            throw new BadRequestException("unknown property: " + key);
        }

        if (!prop.isSettable()) {
            throw new InvalidUpdateException("property '" + prop.getKey() + "' is read-only");
        }
        if (null == rawValue) {
            if (prop.isRequired()) {
                throw new InvalidUpdateException(
                        "property '" + prop.getKey() + "' is required and cannot be set to null");
            }
        }

        V value;
        if (prop.getValueClass().isAssignableFrom(rawValue.getClass())) {
            value = (V) rawValue;
        } else if (rawValue instanceof String && null != prop.getFromString()) {
            value = prop.getFromString().apply((String) rawValue);
        } else {
            throw new InvalidUpdateException("value is not of expected type " + prop.getValueClass().getSimpleName()
                    + ": " + rawValue);
        }

        if (null != prop.getValidator()) {
            try {
                prop.getValidator().consume(value);
            } catch (BadRequestException e) {
                throw new InvalidUpdateException("invalid value for property '" + prop.getKey()
                        + "': " + e.getMessage());
            }
        }

        return value;
    }

    private String trimPage(final String page) {
        String trimmed = page.trim();
        return 0 == trimmed.length() ? null : trimmed;
    }

    @Override
    protected void performTransaction(final ActionContext context) throws RequestProcessingException, BadRequestException {
        Note root = getRoot(getId(), context);
        setFilterParams(context);

        setProperty(root, getName(), getValue());



        switch (getName()) {
            case SemanticSynchrony.PropertyKeys.LABEL:
                root.setLabel((String) value);
                break;
            case SemanticSynchrony.PropertyKeys.TEXT:
                root.setText(trimPage((String) value));
                break;
            case SemanticSynchrony.PropertyKeys.WEIGHT:
                root.setWeight(toFloat(value));
                break;
            case SemanticSynchrony.PropertyKeys.SOURCE:
                root.setSource((String) value);
                break;
            case SemanticSynchrony.PropertyKeys.PRIORITY:
                root.setPriority(toFloat(value));
                context.getBrain().getPriorities().updatePriority(root);
                break;
            case SemanticSynchrony.PropertyKeys.SHORTCUT:
                // first remove this shortcut from any note(s) currently holding it; shortcuts are inverse functional
                String shortcut = (String) value;
                for (Note a : context.getBrain().getTopicGraph().getNotesByShortcut(shortcut, getFilter())) {
                    a.setShortcut(null);
                }

                root.setShortcut(shortcut);
                break;
            default:
                throw new IllegalStateException();
        }

        context.getMap().put("key", context.getBrain().getTopicGraph().idOf(root));
        context.getMap().put("name", getName());
        context.getMap().put("value", value.toString());

        logViewOperation(context, root.getTopic());
    }

    @Override
    protected boolean doesRead() {
        return false;
    }

    @Override
    protected boolean doesWrite() {
        return true;
    }

    private float toFloat(Object uncastDouble) {
        return (float) (double) uncastDouble;
    }
}
