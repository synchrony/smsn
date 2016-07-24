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
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.requests.BasicViewRequest;
import net.fortytwo.smsn.server.SmSnExtension;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * A service for finding root nodes of an Extend-o-Brain graph
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "smsn", name = "find-roots")
//@ExtensionDescriptor(description = "find root nodes of an Extend-o-Brain graph")
public class FindRootsExtension extends SmSnExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for finding root nodes of an Extend-o-Brain graph")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = Params.REQUEST,
                                                   description = "request description (JSON object)") String request) {
        RequestParams p = createParams(context, (KeyIndexableGraph) graph);
        BasicViewRequest r;
        try {
            r = new BasicViewRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        p.height = r.getHeight();
        p.styleName = r.getStyleName();
        p.filter = r.getFilter();

        SemanticSynchrony.logInfo("SmSn find-roots");

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final RequestParams p) throws Exception {
        Note n = p.queries.findRoots(p.filter, p.style, p.height - 1);
        addView(n, p);

        p.map.put("title", "all roots");
        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }
}
