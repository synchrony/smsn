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
import org.json.JSONException;
import org.json.JSONObject;

/**
 * A service for removing isolated atoms (i.e. atoms with neither parents nor children) from an Extend-o-Brain graph
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "remove-isolated-atoms")
public class RemoveIsolatedAtomsExtension extends ExtendoExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for for removing isolated atoms (i.e. atoms with neither parents nor children) from an Extend-o-Brain graph")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "request", description = "request description (JSON object)") String request) {

        Params p = createParams(context, (KeyIndexableGraph) graph);

        FilteredResultsRequest r;
        try {
            r = new FilteredResultsRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        p.filter = r.filter;

        logInfo("extendo remove-isolated-atoms");

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        p.queries.removeIsolatedAtoms(p.filter);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return true;
    }
}
