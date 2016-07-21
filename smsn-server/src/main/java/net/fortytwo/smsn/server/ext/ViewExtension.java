package net.fortytwo.smsn.server.ext;

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
import net.fortytwo.smsn.brain.Note;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.requests.RootedViewRequest;
import net.fortytwo.smsn.server.SmSnExtension;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

/**
 * A service for retrieving hierarchical views of MyOtherBrain graphs
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "smsn", name = "view")
//@ExtensionDescriptor(description = "retrieve a hierarchical view of a MyOtherBrain graph")
public class ViewExtension extends SmSnExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for viewing a portion of a MyOtherBrain graph" +
            " in the wiki format")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = Params.REQUEST,
                                                   description = "request description (JSON object)") String request) {

        RequestParams p = createParams(context, (KeyIndexableGraph) graph);
        ViewRequest r;

        try {
            r = new ViewRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        p.height = r.getHeight();
        p.rootId = r.getRootId();
        p.styleName = r.getStyleName();
        p.filter = r.getFilter();
        p.includeTypes = r.isIncludeTypes();

        SemanticSynchrony.logInfo("SmSn view " + r.getRootId());

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final RequestParams p) throws Exception {

        Note n = p.queries.view(p.root, p.height, p.filter, p.style);
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

        private final boolean includeTypes;

        public ViewRequest(final JSONObject json,
                           final Principal user) throws JSONException {
            super(json, user);

            // this argument is optional; do not include types by default
            includeTypes = json.optBoolean(Params.INCLUDE_TYPES, false);
        }

        public boolean isIncludeTypes() {
            return includeTypes;
        }
    }
}
