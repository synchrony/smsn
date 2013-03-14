package net.fortytwo.extendo.brain.server;

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

/**
 * A service for deriving a prioritized list of items in the knowledge base
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "tinkernotes", name = "priorities")
public class PrioritiesExtension extends TinkerNotesExtension {

    private static final int DEFAULT_MAX_RESULTS = 100;

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for deriving a prioritized list of items in the knowledge base")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "minWeight", description = "minimum-weight criterion for atoms in the view") Float minWeight,
                                           @ExtensionRequestParameter(name = "maxWeight", description = "maximum-weight criterion for atoms in the view") Float maxWeight,
                                           @ExtensionRequestParameter(name = "minSharability", description = "minimum-sharability criterion for atoms in the view") Float minSharability,
                                           @ExtensionRequestParameter(name = "maxSharability", description = "maximum-sharability criterion for atoms in the view") Float maxSharability,
                                           @ExtensionRequestParameter(name = "maxResults", description = "maximum number of results to display") Integer maxResults) {
        logInfo("tinkernotes priorities");
        Params p;

        try {
            p = createParams(context, (KeyIndexableGraph) graph);
            p.filter = createFilter(p.user, minWeight, maxWeight, -1, minSharability, maxSharability, -1);

            if (null == maxResults) {
                maxResults = DEFAULT_MAX_RESULTS;
            } else if (maxResults <= 0) {
                return ExtensionResponse.error("maxResults parameter must be a positive integer");
            }

            p.maxResults = maxResults;
        } catch (Exception e) {
            e.printStackTrace(System.err);
            return ExtensionResponse.error(e);
        }

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {

        Note n = p.queries.priorityView(p.filter, p.maxResults);
        addView(n, p);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }
}
