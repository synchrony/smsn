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
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.requests.FilteredResultsRequest;
import net.fortytwo.smsn.server.SmSnExtension;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * A service for removing isolated atoms (i.e. atoms with neither parents nor children) from an Extend-o-Brain graph
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "smsn", name = "remove-isolated-atoms")
public class RemoveIsolatedAtomsExtension extends SmSnExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for for removing isolated atoms" +
            " (i.e. atoms with neither parents nor children) from an Extend-o-Brain graph")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = Params.REQUEST,
                                                   description = "request description (JSON object)") String request) {

        RequestParams p = createParams(context, (KeyIndexableGraph) graph);

        FilteredResultsRequest r;
        try {
            r = new FilteredResultsRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        p.filter = r.getFilter();

        SemanticSynchrony.logInfo("SmSn remove-isolated-atoms");

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final RequestParams p) throws Exception {
        p.brain.getAtomGraph().removeIsolatedAtoms(p.filter);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return true;
    }
}
