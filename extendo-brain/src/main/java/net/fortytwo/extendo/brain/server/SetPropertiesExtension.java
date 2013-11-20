package net.fortytwo.extendo.brain.server;

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
import net.fortytwo.extendo.brain.ActivityLog;
import net.fortytwo.extendo.brain.BrainGraph;

import java.util.logging.Logger;

/**
 * A service for setting the properties of an atom
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "set")
//@ExtensionDescriptor(description = "set the properties of an atom")
public class SetPropertiesExtension extends ExtendoExtension {
    protected static final Logger LOGGER = Logger.getLogger(SetPropertiesExtension.class.getName());

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for setting properties of given atoms")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "id", description = "id of the atom to be changed") String id,
                                           @ExtensionRequestParameter(name = "name", description = "name of the property to be changed") String name,
                                           @ExtensionRequestParameter(name = "value", description = "value of the property to be changed") Float value) {
        logInfo("extendo set " + id);

        if (name.equals("weight")) {
            // Note: weight may not currently be set to 0, which would cause the atom to disappear from all normal views
            if (value <= 0 || value > 1.0) {
                return ExtensionResponse.error("weight is outside of range (0, 1]: " + value);
            }
        } else if (name.equals("sharability")) {
            if (value <= 0 || value > 1.0) {
                return ExtensionResponse.error("sharability is outside of range (0, 1]: " + value);
            }
        } else if (name.equals("priority")) {
            if (value < 0 || value > 1.0) {
                return ExtensionResponse.error("priority is outside of range [0, 1]: " + value);
            }
        } else {
            return ExtensionResponse.error("unknown property: " + name);
        }

        Params p = createParams(context, (KeyIndexableGraph) graph);
        p.propertyName = name;
        p.propertyValue = value;
        p.rootId = id;

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        if (p.propertyName.equals("weight")) {
            p.root.setWeight(p.propertyValue);
        } else if (p.propertyName.equals("sharability")) {
            p.root.setSharability(p.propertyValue);
        } else if (p.propertyName.equals("priority")) {
            p.root.setPriority(p.propertyValue);
            p.brain.getPriorities().updatePriority(p.root);
        } else {
            throw new IllegalStateException();
        }

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
}
