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
import net.fortytwo.myotherbrain.notes.Note;

import javax.ws.rs.core.SecurityContext;
import java.security.Principal;

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
        LOGGER.info("tinkernotes view " + rootId + " (depth " + depth + ")");
        System.err.println("tinkernotes view " + rootId + " (depth " + depth + ")");

        SecurityContext security = context.getSecurityContext();
        Principal user = null == security ? null : security.getUserPrincipal();

        Filter filter;

        try {
            float m = findMinAuthorizedSharability(user, minSharability);
            filter = new Filter(m, maxSharability, -1, minWeight, maxWeight, -1);
        } catch (IllegalArgumentException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        Params p = new Params();
        p.baseGraph = graph;
        p.depth = depth;
        p.filter = filter;
        p.rootId = rootId;
        p.styleName = styleName;
        ExtensionResponse r =  this.handleRequestInternal(p);

        addToHistory(rootId, context);

        return r;
    }

    @Override
    protected ExtensionResponse performTransaction(final Params p) throws Exception {

        Note n = p.semantics.view(p.root, p.depth, p.filter, p.style);
        addView(n, p);

        return ExtensionResponse.ok(p.map);
    }

    @Override
    protected boolean isReadOnly() {
        return true;
    }
}
