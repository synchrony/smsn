package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.server.Request;
import net.fortytwo.smsn.server.Action;
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
        SetPropertiesRequest r = new SetPropertiesRequest(request, p.user);

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

        p.propertyName = r.name;
        p.propertyValue = r.value;
        p.rootId = r.id;

        SemanticSynchrony.logInfo("SmSn set-properties on " + r.id + ": " + r.name + " <- " + r.value);
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        switch (p.propertyName) {
            case SemanticSynchrony.WEIGHT:
                p.root.setWeight((Float) p.propertyValue);
                break;
            case SemanticSynchrony.SHARABILITY:
                p.root.setSharability((Float) p.propertyValue);
                break;
            case SemanticSynchrony.PRIORITY:
                p.root.setPriority((Float) p.propertyValue);
                p.brain.getPriorities().updatePriority(p.root);
                break;
            case SemanticSynchrony.SHORTCUT:
                // first remove this shortcut from any atom(s) currently holding it; shortcuts are inverse functional
                String shortcut = (String) p.propertyValue;
                for (Atom a : p.brain.getAtomGraph().getAtomsWithShortcut(shortcut, p.filter)) {
                    a.setShortcut(null);
                }

                p.root.setShortcut(shortcut);
                break;
            default:
                throw new IllegalStateException();
        }

        p.brain.getAtomGraph().notifyOfUpdate();

        p.map.put("key", p.brain.getAtomGraph().idOfAtom(p.root));
        p.map.put("name", "" + p.propertyName);
        p.map.put("value", "" + p.propertyValue);

        ActivityLog log = p.brain.getActivityLog();
        if (null != log) {
            log.logSetProperties(p.root);
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
