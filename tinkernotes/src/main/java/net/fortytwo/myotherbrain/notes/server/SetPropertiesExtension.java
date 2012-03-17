package net.fortytwo.myotherbrain.notes.server;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.myotherbrain.MOBGraph;

import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "tinkernotes", name = "set")
public class SetPropertiesExtension extends TinkerNotesExtension {
    protected static final Logger LOGGER = Logger.getLogger(SetPropertiesExtension.class.getName());

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for setting properties of given atoms")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "key", description = "id of the atom to be changed") String id,
                                           @ExtensionRequestParameter(name = "weight", description = "new weight of the atom") Float weight,
                                           @ExtensionRequestParameter(name = "sharability", description = "new sharability of the atom") Float sharability) {
        logInfo("tinkernotes set properties " + id);

        // Note: weight may not currently be set to 0, which would cause the atom to disappear from all normal views
        if (weight <= 0 || weight > 1.0) {
            return ExtensionResponse.error("weight is outside of range (0, 1]: " + weight);
        }

        if (sharability <= 0 || sharability > 1.0) {
            return ExtensionResponse.error("sharability is outside of range (0, 1]: " + sharability);
        }

        Params p = createParams(context, graph);
        p.newWeight = weight;
        p.newSharability = sharability;
        p.rootId = id;

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(Params p) throws Exception {
        p.root.setWeight(p.newWeight);
        p.root.setSharability(p.newSharability);

        p.map.put("key", MOBGraph.getId(p.root));
        p.map.put("weight", "" + p.newWeight);
        p.map.put("sharability", "" + p.newSharability);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return false;
    }

    protected boolean doesWrite() {
        return true;
    }
}
