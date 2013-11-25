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
import com.tinkerpop.rexster.extension.HttpMethod;
import com.tinkerpop.rexster.extension.RexsterContext;

import java.io.IOException;

/**
 * A service for broadcasting events modeled in RDF to all peers in the environment
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "broadcast-rdf")
public class BroadcastRdfService extends ExtendoExtension {

    private final FacilitatorService facilitator;

    public BroadcastRdfService() throws IOException {
        facilitator =  FacilitatorService.getInstance();
    }

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH, method = HttpMethod.POST)
    @ExtensionDescriptor(description = "a service for broadcasting events modeled in RDF to all peers in the environment")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "dataset", description = "a dataset in the TRiG format") String dataset) {
        logInfo("extendo broadcast-rdf");

        Params p = createParams(context, (KeyIndexableGraph) graph);
        p.data = dataset;

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        facilitator.pushUpdate(p.data);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return false;
    }

    protected boolean doesWrite() {
        // pushing of events is currently not considered writing... to the graph
        return false;
    }
}
