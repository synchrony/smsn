package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.actions.requests.SetPropertiesRequest;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for setting the properties of an atom
 */
public class SetProperties extends Action<SetPropertiesRequest> {

    @Override
    public String getName() {
        return "set";
    }

    @Override
    public void parseRequest(final SetPropertiesRequest request, final RequestParams p)
            throws BadRequestException, IOException {

        switch (request.getName()) {
            case SemanticSynchrony.WEIGHT: {
                float f = (Float) request.getValue();
                // Note: weight may not currently be set to 0, which would cause the atom to disappear from all normal views
                if (f <= 0 || f > 1.0) {
                    throw new BadRequestException("weight is outside of range (0, 1]: " + f);
                }
                break;
            }
            case SemanticSynchrony.SHARABILITY: {
                float f = (Float) request.getValue();
                if (f <= 0 || f > 1.0) {
                    throw new BadRequestException("sharability is outside of range (0, 1]: " + f);
                }
                break;
            }
            case SemanticSynchrony.PRIORITY: {
                float f = (Float) request.getValue();
                if (f < 0 || f > 1.0) {
                    throw new BadRequestException("priority is outside of range [0, 1]: " + f);
                }
                break;
            }
            case SemanticSynchrony.SHORTCUT:
                String s = (String) request.getValue();
                if (s.length() > 50) {
                    throw new BadRequestException("shortcut is too long: " + s);
                }
                break;
            default:
                throw new BadRequestException("unknown property: " + request.getName());
        }

        p.setPropertyName(request.getName());
        p.setPropertyValue(request.getValue());
        p.setRootId(request.getId());

        SemanticSynchrony.logInfo("SmSn set-properties on " + request.getId() + ": " + request.getName() + " <- " + request.getValue());
    }

    @Override
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

    @Override
    protected boolean doesRead() {
        return false;
    }

    @Override
    protected boolean doesWrite() {
        return true;
    }

}
