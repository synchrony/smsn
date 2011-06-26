package net.fortytwo.myotherbrain.notes.server;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.frames.FramesManager;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.AbstractRexsterExtension;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.notes.NotesSemantics;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * User: josh
 * Date: 6/19/11
 * Time: 1:40 PM
 */
@ExtensionNaming(namespace = "tinkernotes", name = "set")
public class SetPropertiesExtension extends AbstractRexsterExtension {
    protected static final Logger LOGGER = Logger.getLogger(SetPropertiesExtension.class.getName());

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for setting properties of given atoms")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "key", description = "key of the atom to be changed") String key,
                                           @ExtensionRequestParameter(name = "weight", description = "new weight of the atom") Float weight,
                                           @ExtensionRequestParameter(name = "sharability", description = "new sharability of the atom") Float sharability) {
        LOGGER.info("set properties request for: " + key);

        try {
            if (!(graph instanceof IndexableGraph)) {
                return ExtensionResponse.error("graph must be an instance of IndexableGraph");
            }

            if (weight <= 0 || weight > 1.0) {
                return ExtensionResponse.error("weight is outside of range (0, 1]: " + weight);
            }

            if (sharability <= 0 || sharability > 1.0) {
                return ExtensionResponse.error("sharability is outside of range (0, 1]: " + sharability);
            }

            FramesManager manager = new FramesManager(graph);
            NotesSemantics s = new NotesSemantics((IndexableGraph) graph, manager);

            Map<String, String> map = new HashMap<String, String>();

            Atom a;
            a = s.getAtom(key);
            if (null == a) {
                return ExtensionResponse.error("atom does not exist: " + key);
            }

            a.setWeight(weight);
            a.setSharability(sharability);

            map.put("key", key);
            map.put("weight", "" + weight);
            map.put("sharability", "" + sharability);

            return ExtensionResponse.ok(map);
        } catch (Exception e) {
            // TODO
            e.printStackTrace(System.out);
            return ExtensionResponse.error(e);
        }
    }

}
