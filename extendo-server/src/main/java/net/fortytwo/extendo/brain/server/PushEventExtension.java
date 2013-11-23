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
import net.fortytwo.extendo.brain.Note;
import org.apache.commons.io.IOUtils;
import org.json.JSONObject;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

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
                                           @ExtensionRequestParameter(name = "view", description = "the view of the event to push") String event) {
        logInfo("extendo push-event");

        Params p = createParams(context, (KeyIndexableGraph) graph);
        p.view = event;

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        InputStream in = new ByteArrayInputStream(p.view.getBytes());
        JSONObject j = new JSONObject(IOUtils.toString(in, "UTF-8"));
        Note event = p.parser.fromJSON(j);

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
}
