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
import net.fortytwo.smsn.server.Request;
import net.fortytwo.smsn.server.SmSnExtension;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;
import java.util.List;

/**
 * A service for retrieving the stack of recently pushed events
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "smsn", name = "get-events")
public class GetEventsExtension extends SmSnExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "a service for retrieving the stack of recently pushed events")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = Params.REQUEST,
                                                   description = "request description (JSON object)") String request) {
        RequestParams p = createParams(context, (KeyIndexableGraph) graph);
        GetEventsRequest r;
        try {
            r = new GetEventsRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        p.height = r.height;

        SemanticSynchrony.logInfo("SmSn get-events");

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final RequestParams p) throws Exception {
        List<Note> events = p.brain.getEventStack().getEvents();

        Note view = new Note();
        view.setValue("event stack");

        for (Note n : events) {
            Note e = new Note(n);
            e.truncate(p.height);
            view.addChild(e);
        }

        addView(view, p);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        // getting events is currently not considered reading... from the graph
        return false;
    }

    protected boolean doesWrite() {
        return false;
    }

    protected class GetEventsRequest extends Request {
        public final int height;

        public GetEventsRequest(JSONObject json, Principal user) throws JSONException {
            super(json, user);

            height = this.json.getInt(Params.HEIGHT);
        }
    }
}
