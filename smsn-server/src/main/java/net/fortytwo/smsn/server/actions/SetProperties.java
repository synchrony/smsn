package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

/**
 * A service for setting the properties of a note
 */
public class SetProperties extends FilteredAction {

    private AtomId id;
    private String name;
    private Object value;

    private AtomId getId() {
        return notNull(id);
    }

    private String getName() {
        return notNull(name);
    }

    private Object getValue() {
        return notNull(value);
    }

    public void setId(AtomId id) {
        this.id = id;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    private void validateKeyValue() {
        switch (getName()) {
            case SemanticSynchrony.PropertyKeys.TITLE:
                validateTitle();
                break;
            case SemanticSynchrony.PropertyKeys.TEXT:
                // nothing to do; every Markdown page is valid
                break;
            case SemanticSynchrony.PropertyKeys.WEIGHT:
                validateWeight();
                break;
            case SemanticSynchrony.PropertyKeys.SOURCE:
                validateSource();
                break;
            case SemanticSynchrony.PropertyKeys.PRIORITY:
                validatePriority();
                break;
            case SemanticSynchrony.PropertyKeys.SHORTCUT:
                validateShortcut();
                break;
            default:
                throw new BadRequestException("unknown property: " + name);
        }
    }

    private void validateTitle() {
        if (((String) getValue()).trim().length() == 0) {
            throw new BadRequestException("empty value");
        }
    }

    private void validateWeight() {
        float f = toFloat(getValue());
        // Note: weight may not currently be set to 0, which would cause the note to disappear from all normal views
        if (f <= 0 || f > 1.0) {
            throw new BadRequestException("weight is outside of range (0, 1]: " + f);
        }
    }

    private void validateSource() {
        String source = (String) getValue();
        if (source.trim().length() == 0) {
            throw new BadRequestException("empty source");
        }
    }

    private void validatePriority() {
        float f = toFloat(getValue());
        if (f < 0 || f > 1.0) {
            throw new BadRequestException("priority is outside of range [0, 1]: " + f);
        }
    }

    private void validateShortcut() {
        String s = (String) getValue();
        if (s.length() > 50) {
            throw new BadRequestException("shortcut is too long: " + s);
        }
    }

    private String trimPage(final String page) {
        String trimmed = page.trim();
        return 0 == trimmed.length() ? null : trimmed;
    }

    @Override
    protected void performTransaction(final ActionContext context) throws RequestProcessingException, BadRequestException {
        validateKeyValue();

        AtomId atomId = getId();
        String propertyKey = getName();
        Object value = getValue();

        // Special handling for shortcuts - must be unique across the graph
        if (SemanticSynchrony.PropertyKeys.SHORTCUT.equals(propertyKey)) {
            String shortcut = (String) value;
            // Remove this shortcut from any other atoms currently holding it
            for (net.fortytwo.smsn.brain.Atom atom : context.getRepository().findByShortcut(shortcut, getFilter())) {
                if (!atom.id.equals(atomId)) {
                    context.getRepository().updateProperty(atom.id, SemanticSynchrony.PropertyKeys.SHORTCUT, null);
                }
            }
        }

        // Trim text values
        if (SemanticSynchrony.PropertyKeys.TEXT.equals(propertyKey)) {
            value = trimPage((String) value);
        }

        // Update the property using AtomRepository
        context.getRepository().updateProperty(atomId, propertyKey, value);

        context.getMap().put("key", atomId);
        context.getMap().put("name", propertyKey);
        context.getMap().put("value", value != null ? value.toString() : "");

        // Activity logging
        ActivityLog log = context.getActivityLog();
        if (null != log) {
            log.logSetPropertiesById(atomId);
        }
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
