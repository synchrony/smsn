package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import javax.validation.constraints.NotNull;
import java.io.IOException;

/**
 * A service for setting the properties of an atom
 */
public class SetProperties extends Action {

    @NotNull
    private String id;
    @NotNull
    private String name;
    @NotNull
    private Object value;

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public Object getValue() {
        return value;
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

    @Override
    public void parseRequest(final RequestParams params)
            throws BadRequestException, IOException {

        validateKeyValue();

        params.setPropertyName(getName());
        params.setPropertyValue(getValue());
        params.setRootId(getId());

        SemanticSynchrony.logInfo("SmSn set-properties on " + getId() + ": " + getName() + " <- " + getValue());
    }

    private void validateKeyValue() {
        switch (getName()) {
            case SemanticSynchrony.TITLE:
                validateValue();
                break;
            case SemanticSynchrony.WEIGHT:
                validateWeight();
                break;
            case SemanticSynchrony.SHARABILITY:
                validateSharability();
                break;
            case SemanticSynchrony.PRIORITY:
                validatePriority();
                break;
            case SemanticSynchrony.SHORTCUT:
                validateShortcut();
                break;
            default:
                throw new BadRequestException("unknown property: " + getName());
        }
    }

    private void validateValue() {
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

    private void validateSharability() {
        float f = toFloat(getValue());
        if (f <= 0 || f > 1.0) {
            throw new BadRequestException("sharability is outside of range (0, 1]: " + f);
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

    @Override
    protected void performTransaction(final RequestParams params) throws RequestProcessingException, BadRequestException {
        switch (params.getPropertyName()) {
            case SemanticSynchrony.TITLE:
                params.getRoot().setTitle((String) params.getPropertyValue());
                break;
            case SemanticSynchrony.WEIGHT:
                params.getRoot().setWeight(toFloat(params.getPropertyValue()));
                break;
            case SemanticSynchrony.SHARABILITY:
                params.getRoot().setSharability(toFloat(params.getPropertyValue()));
                break;
            case SemanticSynchrony.PRIORITY:
                params.getRoot().setPriority(toFloat(params.getPropertyValue()));
                params.getBrain().getPriorities().updatePriority(params.getRoot());
                break;
            case SemanticSynchrony.SHORTCUT:
                // first remove this shortcut from any atom(s) currently holding it; shortcuts are inverse functional
                String shortcut = (String) params.getPropertyValue();
                for (Atom a : params.getBrain().getAtomGraph().getAtomsByShortcut(shortcut, params.getFilter())) {
                    a.setShortcut(null);
                }

                params.getRoot().setShortcut(shortcut);
                break;
            default:
                throw new IllegalStateException();
        }

        params.getBrain().getAtomGraph().reindexAtom(params.getRoot());
        params.getBrain().getAtomGraph().notifyOfUpdate();

        params.getMap().put("key", params.getBrain().getAtomGraph().idOfAtom(params.getRoot()));
        params.getMap().put("name", "" + params.getPropertyName());
        params.getMap().put("value", "" + params.getPropertyValue());

        ActivityLog log = params.getBrain().getActivityLog();
        if (null != log) {
            log.logSetProperties(params.getRoot());
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
