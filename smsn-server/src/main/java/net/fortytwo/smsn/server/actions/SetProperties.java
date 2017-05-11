package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import javax.validation.constraints.NotNull;

/**
 * A service for setting the properties of an atom
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

    private void validateKeyValue() {
        switch (getName()) {
            case SemanticSynchrony.PropertyKeys.TITLE:
                validateTitle();
                break;
            case SemanticSynchrony.PropertyKeys.PAGE:
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
        // Note: weight may not currently be set to 0, which would cause the atom to disappear from all normal views
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
    protected void performTransaction(final ActionContext params) throws RequestProcessingException, BadRequestException {
        validateKeyValue();

        Atom root = getRoot(getId(), params);
        Object value = getValue();

        switch (getName()) {
            case SemanticSynchrony.PropertyKeys.TITLE:
                root.setTitle((String) value);
                break;
            case SemanticSynchrony.PropertyKeys.PAGE:
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
                params.getBrain().getPriorities().updatePriority(root);
                break;
            case SemanticSynchrony.PropertyKeys.SHORTCUT:
                // first remove this shortcut from any atom(s) currently holding it; shortcuts are inverse functional
                String shortcut = (String) value;
                for (Atom a : params.getBrain().getTopicGraph().getAtomsByShortcut(shortcut, getFilter())) {
                    a.setShortcut(null);
                }

                root.setShortcut(shortcut);
                break;
            default:
                throw new IllegalStateException();
        }

        params.getBrain().getTopicGraph().notifyOfUpdate();

        params.getMap().put("key", params.getBrain().getTopicGraph().idOfAtom(root));
        params.getMap().put("name", getName());
        params.getMap().put("value", value.toString());

        ActivityLog log = params.getBrain().getActivityLog();
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
