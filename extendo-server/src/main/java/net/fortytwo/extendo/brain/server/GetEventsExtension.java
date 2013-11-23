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

import java.util.List;

/**
 * A service for retrieving the stack of recently pushed events
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "get-events")
public class GetEventsExtension extends ExtendoExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "a service for retrieving the stack of recently pushed events")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "depth", description = "depth of the event view") Integer depth) {
        logInfo("extendo get-events");

        Params p = createParams(context, (KeyIndexableGraph) graph);
        p.depth = depth;

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        List<Note> events = p.brain.getEventStack().getEvents();

        // TODO: temporary, for debugging
        if (0 == events.size()) {
            Note debugNote = new Note();
            debugNote.setValue("test event");
            debugNote.setWeight(0.5f);
            debugNote.setSharability(0.5f);
            p.brain.getEventStack().push(debugNote);
        }

        Note view = new Note();
        view.setValue("event stack");

        for (Note n : events) {
            Note e = new Note(n);
            e.truncate(p.depth);
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
}
