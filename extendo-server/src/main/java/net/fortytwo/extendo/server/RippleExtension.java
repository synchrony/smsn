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
import net.fortytwo.extendo.brain.Note;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

/**
 * A service for executing Ripple queries over Extend-o-Brain graphs
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "ripple")
//@ExtensionDescriptor(description = "execute a Ripple query over an Extend-o-Brain graph")
public class RippleExtension extends ExtendoExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for performing Ripple queries over Extend-o-Brain graphs")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "request", description = "request description (JSON object)") String request) {
        Params p = createParams(context, (KeyIndexableGraph) graph);
        BasicSearchRequest r;
        try {
            r = new BasicSearchRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        p.depth = r.getDepth();
        p.query = r.getQuery();
        p.styleName = r.getStyleName();
        p.filter = r.getFilter();

        logInfo("extendo ripple: \"" + r.getQuery() + "\"");

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        addSearchResults(p);

        p.map.put("title", p.query);
        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }

    protected void addSearchResults(final Params p) throws IOException {
        // TODO: restore Ripple after dealing with Android/Dalvik + dependency issues
        Note n = new Note();
        //Note n = p.queries.rippleQuery(p.query, p.depth, p.filter, p.style);
        JSONObject json;

        try {
            json = p.writer.toJSON(n);
        } catch (JSONException e) {
            throw new IOException(e);
        }
        p.map.put("view", json.toString());
    }
}
