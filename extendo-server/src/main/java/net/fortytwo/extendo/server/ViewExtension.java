package net.fortytwo.extendo.server;

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
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.extendo.brain.rdf.KnowledgeBase;
import org.json.JSONException;

import java.security.Principal;

/**
 * A service for retrieving hierarchical views of Extend-o-Brain graphs
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "view")
//@ExtensionDescriptor(description = "retrieve a hierarchical view of an Extend-o-Brain graph")
public class ViewExtension extends ExtendoExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for viewing a portion of an Extend-o-Brain graph in the Extendo Wiki format")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "request", description = "request description (JSON object)") String request) {

        Params p = createParams(context, (KeyIndexableGraph) graph);
        ViewRequest r;

        try {
            r = new ViewRequest(request, p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        p.depth = r.depth;
        p.rootId = r.rootId;
        p.styleName = r.styleName;
        p.filter = r.filter;
        p.includeTypes = r.includeTypes;

        logInfo("extendo view " + r.rootId);

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {

        Note n = p.queries.view(p.root, p.depth, p.filter, p.style);
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

    private class ViewRequest extends RootedViewRequest {
        public final boolean includeTypes;

        public ViewRequest(final String jsonStr,
                           final Principal user) throws JSONException {
            super(jsonStr, user);

            // this argument is optional; do not include types by default
            includeTypes = json.optBoolean(INCLUDE_TYPES, false);
        }
    }
}
