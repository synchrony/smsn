package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.server.Request;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

/**
 * A service for setting the properties of an atom
 */
public class SetProperties extends Action {

    @Override
    public String getName() {
        return "set";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException, BadRequestException {
        SetPropertiesRequest r = new SetPropertiesRequest(request, p.getUser());

        switch (r.name) {
            case SemanticSynchrony.WEIGHT: {
                float f = (Float) r.value;
                // Note: weight may not currently be set to 0, which would cause the atom to disappear from all normal views
                if (f <= 0 || f > 1.0) {
                    throw new BadRequestException("weight is outside of range (0, 1]: " + f);
                }
                break;
            }
            case SemanticSynchrony.SHARABILITY: {
                float f = (Float) r.value;
                if (f <= 0 || f > 1.0) {
                    throw new BadRequestException("sharability is outside of range (0, 1]: " + f);
                }
                break;
            }
            case SemanticSynchrony.PRIORITY: {
                float f = (Float) r.value;
                if (f < 0 || f > 1.0) {
                    throw new BadRequestException("priority is outside of range [0, 1]: " + f);
                }
                break;
            }
            case SemanticSynchrony.SHORTCUT:
                String s = (String) r.value;
                if (s.length() > 50) {
                    throw new BadRequestException("shortcut is too long: " + s);
                }
                break;
            default:
                throw new BadRequestException("unknown property: " + r.name);
        }

        p.setPropertyName(r.name);
        p.setPropertyValue(r.value);
        p.setRootId(r.id);

        SemanticSynchrony.logInfo("SmSn set-properties on " + r.id + ": " + r.name + " <- " + r.value);
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        switch (p.getPropertyName()) {
            case SemanticSynchrony.WEIGHT:
                p.getRoot().setWeight((Float) p.getPropertyValue());
                break;
            case SemanticSynchrony.SHARABILITY:
                p.getRoot().setSharability((Float) p.getPropertyValue());
                break;
            case SemanticSynchrony.PRIORITY:
                p.getRoot().setPriority((Float) p.getPropertyValue());
                p.getBrain().getPriorities().updatePriority(p.getRoot());
                break;
            case SemanticSynchrony.SHORTCUT:
                // first remove this shortcut from any atom(s) currently holding it; shortcuts are inverse functional
                String shortcut = (String) p.getPropertyValue();
                for (Atom a : p.getBrain().getAtomGraph().getAtomsByShortcut(shortcut, p.getFilter())) {
                    a.setShortcut(null);
                }

                p.getRoot().setShortcut(shortcut);
                break;
            default:
                throw new IllegalStateException();
        }

        p.getBrain().getAtomGraph().notifyOfUpdate();

        p.getMap().put("key", p.getBrain().getAtomGraph().idOfAtom(p.getRoot()));
        p.getMap().put("name", "" + p.getPropertyName());
        p.getMap().put("value", "" + p.getPropertyValue());

        ActivityLog log = p.getBrain().getActivityLog();
        if (null != log) {
            log.logSetProperties(p.getRoot());
        }
    }

    protected boolean doesRead() {
        return false;
    }

    protected boolean doesWrite() {
        return true;
    }

    protected class SetPropertiesRequest extends Request {
        public final String id;
        public final String name;
        public final Object value;

        public SetPropertiesRequest(JSONObject json, Principal user) throws JSONException {
            super(json, user);

            id = this.json.getString(Params.ID);
            name = this.json.getString(Params.NAME);
            value = name.equals(SemanticSynchrony.SHORTCUT)
                    ? this.json.getString(Params.VALUE)
                    : (float) this.json.getDouble(Params.VALUE);
        }
    }

}
