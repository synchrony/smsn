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
import com.tinkerpop.rexster.extension.HttpMethod;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.extendo.util.properties.PropertyException;
import net.fortytwo.ripple.RippleException;
import org.json.JSONException;
import org.json.JSONObject;
import org.openrdf.rio.RDFFormat;
import org.openrdf.sail.SailException;

import java.io.IOException;
import java.security.Principal;

/**
 * A service for broadcasting events modeled in RDF to all peers in the environment
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "broadcast-rdf")
public class BroadcastRdfExtension extends ExtendoExtension {

    private final FacilitatorService facilitator;

    public BroadcastRdfExtension() throws IOException, PropertyException, RippleException, SailException {
        facilitator =  FacilitatorService.getInstance();
    }

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH, method = HttpMethod.POST)
    @ExtensionDescriptor(description = "a service for broadcasting events modeled in RDF to all peers in the environment")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "request", description = "request description (JSON object)") String request) {
        Params p = createParams(context, (KeyIndexableGraph) graph);
        BroadcastRdfRequest r;
        try {
            r = new BroadcastRdfRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        p.data = r.dataset;

        logInfo("extendo broadcast-rdf");

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        // TODO: take RDF format as an input parameter
        RDFFormat format = RDFFormat.NTRIPLES;

        facilitator.pushUpdate(p.data, format);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return false;
    }

    protected boolean doesWrite() {
        // pushing of events is currently not considered writing... to the graph
        return false;
    }

    protected class BroadcastRdfRequest extends Request {
        public final String dataset;

        public BroadcastRdfRequest(JSONObject json, Principal user) throws JSONException {
            super(json, user);

            dataset = this.json.getString(DATASET);
        }
    }
}
