package net.fortytwo.smsn.server;

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
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.security.Principal;

/**
 * A service for executing keyword search over an Extend-o-Brain graph
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "smsn", name = "search")
//@ExtensionDescriptor(description = "execute keyword search over an Extend-o-Brain graph")
public class SearchExtension extends SmSnExtension {

    private static final int DEFAULT_VALUE_LENGTH_CUTOFF = 100;

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for performing full text search over an Extend-o-Brain graph")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = Params.REQUEST,
                                                   description = "request description (JSON object)") String request) {
        RequestParams p = createParams(context, (KeyIndexableGraph) graph);
        SearchRequest r;
        try {
            r = new SearchRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        p.height = r.getHeight();
        p.queryType = r.getQueryType();
        p.query = r.getQuery();
        p.styleName = r.getStyleName();
        p.filter = r.getFilter();
        p.valueCutoff = r.valueCutoff;

        SemanticSynchrony.logInfo("SmSn search: \"" + r.getQuery() + "\"");

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final RequestParams p) throws Exception {
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

    protected void addSearchResults(final RequestParams p) throws IOException {
        Note n = p.queries.search(p.queryType, p.query, p.height, p.filter, p.style);
        addView(n, p);
    }

    protected class SearchRequest extends BasicSearchRequest {
        public final int valueCutoff;

        public SearchRequest(JSONObject json, Principal user) throws JSONException {
            super(json, user);

            valueCutoff = this.json.getInt(Params.VALUE_CUTOFF);
        }
    }
}
