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
import net.fortytwo.myotherbrain.notes.Filter;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "tinkernotes", name = "view")
public class ViewExtension extends TinkerNotesExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for viewing a portion of a MyOtherBrain graph in the TinkerNotes format")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "root", description = "root atom (vertex) of the view") String rootKey,
                                           @ExtensionRequestParameter(name = "depth", description = "depth of the view") Integer depth,
                                           @ExtensionRequestParameter(name = "minSharability", description = "minimum-sharability criterion for atoms in the view") Float minSharability,
                                           @ExtensionRequestParameter(name = "maxSharability", description = "maximum-sharability criterion for atoms in the view") Float maxSharability,
                                           @ExtensionRequestParameter(name = "minWeight", description = "minimum-weight criterion for atoms in the view") Float minWeight,
                                           @ExtensionRequestParameter(name = "maxWeight", description = "maximum-weight criterion for atoms in the view") Float maxWeight,
                                           @ExtensionRequestParameter(name = "style", description = "the style of view to generate") String styleName) {
        LOGGER.info("view request for: " + rootKey);
        System.err.println("view request for: " + rootKey);

        Filter filter = new Filter(minSharability, maxSharability, minWeight, maxWeight);

        Params p = new Params();
        p.graph = graph;
        p.depth = depth;
        p.filter = filter;
        return this.handleRequestInternal(p, rootKey, styleName);
    }

    @Override
    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        addView(p);

        return ExtensionResponse.ok(p.map);
    }

    @Override
    protected boolean isReadOnly() {
        return true;
    }
}
