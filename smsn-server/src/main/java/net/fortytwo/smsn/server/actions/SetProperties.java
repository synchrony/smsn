package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

/**
 * A service for setting the properties of a note
 */
public class SetProperties extends FilteredAction {

    private String id;
    private String name;
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

        Note root = getRoot(getId(), context);
        setFilterParams(context);
        Object value = getValue();

        switch (getName()) {
            case SemanticSynchrony.PropertyKeys.TITLE:
                Note.setTitle(root, (String) value);
                break;
            case SemanticSynchrony.PropertyKeys.TEXT:
                Note.setText(root, trimPage((String) value));
                break;
            case SemanticSynchrony.PropertyKeys.WEIGHT:
                Note.setWeight(root, toFloat(value));
                break;
            case SemanticSynchrony.PropertyKeys.SOURCE:
                Note.setSource(root, (String) value);
                break;
            case SemanticSynchrony.PropertyKeys.PRIORITY:
                Note.setPriority(root, toFloat(value));
                context.getBrain().getPriorities().updatePriority(root);
                break;
            case SemanticSynchrony.PropertyKeys.SHORTCUT:
                // first remove this shortcut from any note(s) currently holding it; shortcuts are inverse functional
                String shortcut = (String) value;
                for (Note a : context.getBrain().getTopicGraph().getNotesByShortcut(shortcut, getFilter())) {
                    Note.setShortcut(a, null);
                }

                Note.setShortcut(root, shortcut);
                break;
            default:
                throw new IllegalStateException();
        }

        context.getBrain().getTopicGraph().notifyOfUpdate();

        context.getMap().put("key", context.getBrain().getTopicGraph().idOf(root));
        context.getMap().put("name", getName());
        context.getMap().put("value", value.toString());

        ActivityLog log = context.getBrain().getActivityLog();
        if (null != log) {
            log.logSetProperties(root);
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
