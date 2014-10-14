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
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Note;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

/**
 * A service for deriving a prioritized list of items in the knowledge base
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "priorities")
public class PrioritiesExtension extends ExtendoExtension {

    private static final int DEFAULT_MAX_RESULTS = 100;

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for deriving a prioritized list of items in the knowledge base")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "request", description = "request description (JSON object)") String request) {
        Params p = createParams(context, (KeyIndexableGraph) graph);
        PrioritiesRequest r;
        try {
            r = new PrioritiesRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        p.filter = r.getFilter();
        p.maxResults = r.maxResults;

        Extendo.logInfo("extendo priorities");

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {

        Note n = p.queries.priorityView(p.filter, p.maxResults, p.brain.getPriorities());
        addView(n, p);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }

    protected class PrioritiesRequest extends FilteredResultsRequest {
        public final int maxResults;

        public PrioritiesRequest(JSONObject json, Principal user) throws JSONException {
            super(json, user);

            maxResults = this.json.optInt(MAX_RESULTS, DEFAULT_MAX_RESULTS);

            if (maxResults <= 0) {
                throw new JSONException(MAX_RESULTS + " parameter must be a positive integer");
            }
        }
    }
}
