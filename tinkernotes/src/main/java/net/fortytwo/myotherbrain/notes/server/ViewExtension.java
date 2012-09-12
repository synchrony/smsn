package net.fortytwo.myotherbrain.notes.server;

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
import net.fortytwo.myotherbrain.notes.Note;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "tinkernotes", name = "view")
public class ViewExtension extends TinkerNotesExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for viewing a portion of a MyOtherBrain graph in the TinkerNotes format")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "root", description = "root atom (vertex) of the view") String rootId,
                                           @ExtensionRequestParameter(name = "depth", description = "depth of the view") Integer depth,
                                           @ExtensionRequestParameter(name = "minWeight", description = "minimum-weight criterion for atoms in the view") Float minWeight,
                                           @ExtensionRequestParameter(name = "maxWeight", description = "maximum-weight criterion for atoms in the view") Float maxWeight,
                                           @ExtensionRequestParameter(name = "minSharability", description = "minimum-sharability criterion for atoms in the view") Float minSharability,
                                           @ExtensionRequestParameter(name = "maxSharability", description = "maximum-sharability criterion for atoms in the view") Float maxSharability,
                                           @ExtensionRequestParameter(name = "style", description = "the style of view to generate") String styleName) {
        logInfo("tinkernotes view " + rootId + " (depth " + depth + ")");

        Params p = createParams(context, (KeyIndexableGraph) graph);
        p.depth = depth;
        p.rootId = rootId;
        p.styleName = styleName;
        p.filter = createFilter(p.user, minWeight, maxWeight, -1, minSharability, maxSharability, -1);

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {

        Note n = p.queries.view(p.root, p.depth, p.filter, p.style, p.graph.getActivityLog());
        addView(n, p);

        addToHistory(p.rootId, p.context);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }
}
