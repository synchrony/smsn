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

import java.io.IOException;
import java.security.Principal;

/**
 * A service for executing keyword search over an Extend-o-Brain graph
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "search")
//@ExtensionDescriptor(description = "execute keyword search over an Extend-o-Brain graph")
public class SearchExtension extends ExtendoExtension {

    private static final int DEFAULT_VALUE_LENGTH_CUTOFF = 100;

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for performing full text search over an Extend-o-Brain graph")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "request", description = "request description (JSON object)") String request) {
        Params p = createParams(context, (KeyIndexableGraph) graph);
        SearchRequest r;
        try {
            r = new SearchRequest(request, p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        //logInfo("extendo search \"" + query + "\"");

        p.depth = r.depth;
        p.query = r.query;
        p.styleName = r.styleName;
        p.filter = r.filter;
        p.valueCutoff = r.valueCutoff;

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        if (null == p.valueCutoff) {
            p.writer.setValueLengthCutoff(DEFAULT_VALUE_LENGTH_CUTOFF);
        } else {
            p.writer.setValueLengthCutoff(p.valueCutoff);
        }

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
        Note n = p.queries.search(p.query, p.depth, p.filter, p.style);
        addView(n, p);
    }

    protected class SearchRequest extends BasicSearchRequest {
        public final int valueCutoff;

        public SearchRequest(String jsonStr, Principal user) throws JSONException {
            super(jsonStr, user);

            valueCutoff = json.getInt(VALUE_CUTOFF);
        }
    }
}
