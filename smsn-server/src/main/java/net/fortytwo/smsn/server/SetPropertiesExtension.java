package net.fortytwo.smsn.server;

import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.BrainGraph;
import net.fortytwo.smsn.brain.Params;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;
import java.util.logging.Logger;

/**
 * A service for setting the properties of an atom
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "smsn", name = "set")
//@ExtensionDescriptor(description = "set the properties of an atom")
public class SetPropertiesExtension extends SmSnExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for setting properties of given atoms")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = Params.REQUEST,
                                                   description = "request description (JSON object)") String request) {
        RequestParams p = createParams(context, (KeyIndexableGraph) graph);
        SetPropertiesRequest r;
        try {
            r = new SetPropertiesRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.badRequest(e.getMessage(), null);
        }

        if (r.name.equals(SemanticSynchrony.WEIGHT)) {
            float f = (Float) r.value;
            // Note: weight may not currently be set to 0, which would cause the atom to disappear from all normal views
            if (f <= 0 || f > 1.0) {
                return ExtensionResponse.badRequest("weight is outside of range (0, 1]: " + f, null);
            }
        } else if (r.name.equals(SemanticSynchrony.SHARABILITY)) {
            float f = (Float) r.value;
            if (f <= 0 || f > 1.0) {
                return ExtensionResponse.badRequest("sharability is outside of range (0, 1]: " + f, null);
            }
        } else if (r.name.equals(SemanticSynchrony.PRIORITY)) {
            float f = (Float) r.value;
            if (f < 0 || f > 1.0) {
                return ExtensionResponse.badRequest("priority is outside of range [0, 1]: " + f, null);
            }
        } else if (r.name.equals(SemanticSynchrony.SHORTCUT)) {
            String s = (String) r.value;
            if (s.length() > 50) {
                return ExtensionResponse.badRequest("shortcut is too long: " + s, null);
            }
        } else {
            return ExtensionResponse.error("unknown property: " + r.name);
        }

        p.propertyName = r.name;
        p.propertyValue = r.value;
        p.rootId = r.id;

        SemanticSynchrony.logInfo("SmSn set-properties on " + r.id + ": " + r.name + " <- " + r.value);

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final RequestParams p) throws Exception {
        if (p.propertyName.equals(SemanticSynchrony.WEIGHT)) {
            p.root.setWeight((Float) p.propertyValue);
        } else if (p.propertyName.equals(SemanticSynchrony.SHARABILITY)) {
            p.root.setSharability((Float) p.propertyValue);
        } else if (p.propertyName.equals(SemanticSynchrony.PRIORITY)) {
            p.root.setPriority((Float) p.propertyValue);
            p.brain.getPriorities().updatePriority(p.root);
        } else if (p.propertyName.equals(SemanticSynchrony.SHORTCUT)) {
            // first remove this shortcut from any atom(s) currently holding it; shortcuts are inverse functional
            String shortcut = (String) p.propertyValue;
            for (Atom a : p.brain.getBrainGraph().getAtomsWithShortcut(shortcut, p.filter)) {
                a.setShortcut(null);
            }

            p.root.setShortcut(shortcut);
        } else {
            throw new IllegalStateException();
        }

        p.brain.getBrainGraph().updated();

        p.map.put("key", BrainGraph.getId(p.root));
        p.map.put("name", "" + p.propertyName);
        p.map.put("value", "" + p.propertyValue);

        ActivityLog log = p.brain.getActivityLog();
        if (null != log) {
            log.logSetProperties(p.root);
        }

        return ExtensionResponse.ok(p.map);
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
