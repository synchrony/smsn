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
import net.fortytwo.extendo.brain.Note;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

/**
 * A service for receiving and internalizing events
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "push-event")
public class PushEventExtension extends ExtendoExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH, method = HttpMethod.POST)
    @ExtensionDescriptor(description = "a service for receiving and internalizing events")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "request", description = "request description (JSON object)") String request) {
        Params p = createParams(context, (KeyIndexableGraph) graph);
        PushEventRequest r;
        try {
            r = new PushEventRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        p.jsonView = r.jsonView;

        logInfo("extendo push-event");

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        Note event = p.parser.fromJSON(p.jsonView);

        p.brain.getEventStack().push(event);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return false;
    }

    protected boolean doesWrite() {
        // pushing of events is currently not considered writing... to the graph
        return false;
    }

    protected class PushEventRequest extends Request {
        public final JSONObject jsonView;

        public PushEventRequest(JSONObject jsonStr, Principal user) throws JSONException {
            super(jsonStr, user);

            jsonView = json.getJSONObject(VIEW);
        }
    }
}
