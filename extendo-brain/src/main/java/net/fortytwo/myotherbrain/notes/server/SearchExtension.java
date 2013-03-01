package net.fortytwo.myotherbrain.notes.server;

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
import net.fortytwo.myotherbrain.notes.Note;

import java.io.IOException;
import java.io.UnsupportedEncodingException;

/**
 * A service for executing keyword search over a TinkerNotes graph
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "tinkernotes", name = "search")
//@ExtensionDescriptor(description = "execute keyword search over a TinkerNotes graph")
public class SearchExtension extends TinkerNotesExtension {

    private static final int DEFAULT_VALUE_LENGTH_CUTOFF = 100;

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for performing full text search over MyOtherBrain using TinkerNotes")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "query", description = "full-text query") String query,
                                           @ExtensionRequestParameter(name = "depth", description = "depth of the view") Integer depth,
                                           @ExtensionRequestParameter(name = "minWeight", description = "minimum-weight criterion for atoms in the view") Float minWeight,
                                           @ExtensionRequestParameter(name = "maxWeight", description = "maximum-weight criterion for atoms in the view") Float maxWeight,
                                           @ExtensionRequestParameter(name = "minSharability", description = "minimum-sharability criterion for atoms in the view") Float minSharability,
                                           @ExtensionRequestParameter(name = "maxSharability", description = "maximum-sharability criterion for atoms in the view") Float maxSharability,
                                           @ExtensionRequestParameter(name = "style", description = "the style of view to generate") String styleName,
                                           @ExtensionRequestParameter(name = "valueCutoff", description = "cutoff for long values") Integer valueCutoff) {
        try {
            // TODO: this doesn't solve the problem (that you can't search on queries with extended characters)
            query = new String(query.getBytes(), "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException(e);
        }

        logInfo("tinkernotes search \"" + query + "\"");

        Params p = createParams(context, (KeyIndexableGraph) graph);
        p.depth = depth;
        p.query = query;
        p.styleName = styleName;
        p.filter = createFilter(p.user, minWeight, maxWeight, -1, minSharability, maxSharability, -1);

        if (null == valueCutoff || valueCutoff <= 0) {
            p.writer.setValueLengthCutoff(DEFAULT_VALUE_LENGTH_CUTOFF);
        } else {
            p.writer.setValueLengthCutoff(valueCutoff);
        }

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
        Note n = p.queries.search(p.query, p.depth, p.filter, p.style);
        addView(n, p);
    }
}
