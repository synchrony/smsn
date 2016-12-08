package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.action.requests.SetPropertiesRequest;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

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

        switch (r.getName()) {
            case SemanticSynchrony.WEIGHT: {
                float f = (Float) r.getValue();
                // Note: weight may not currently be set to 0, which would cause the atom to disappear from all normal views
                if (f <= 0 || f > 1.0) {
                    throw new BadRequestException("weight is outside of range (0, 1]: " + f);
                }
                break;
            }
            case SemanticSynchrony.SHARABILITY: {
                float f = (Float) r.getValue();
                if (f <= 0 || f > 1.0) {
                    throw new BadRequestException("sharability is outside of range (0, 1]: " + f);
                }
                break;
            }
            case SemanticSynchrony.PRIORITY: {
                float f = (Float) r.getValue();
                if (f < 0 || f > 1.0) {
                    throw new BadRequestException("priority is outside of range [0, 1]: " + f);
                }
                break;
            }
            case SemanticSynchrony.SHORTCUT:
                String s = (String) r.getValue();
                if (s.length() > 50) {
                    throw new BadRequestException("shortcut is too long: " + s);
                }
                break;
            default:
                throw new BadRequestException("unknown property: " + r.getName());
        }

        p.setPropertyName(r.getName());
        p.setPropertyValue(r.getValue());
        p.setRootId(r.getId());

        SemanticSynchrony.logInfo("SmSn set-properties on " + r.getId() + ": " + r.getName() + " <- " + r.getValue());
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

}
