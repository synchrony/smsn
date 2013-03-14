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
import net.fortytwo.extendo.brain.NoteQueries;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

/**
 * A service for updating a TinkerNotes graph
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "tinkernotes", name = "update")
//@ExtensionDescriptor(description = "update a TinkerNotes graph")
public class UpdateExtension extends TinkerNotesExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH, method = HttpMethod.POST)
    @ExtensionDescriptor(description = "update a TinkerNotes graph")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "root", description = "root atom (vertex) of the view") String rootId,
                                           @ExtensionRequestParameter(name = "depth", description = "depth of the view") Integer depth,
                                           @ExtensionRequestParameter(name = "minWeight", description = "minimum-weight criterion for atoms in the view") Float minWeight,
                                           @ExtensionRequestParameter(name = "maxWeight", description = "maximum-weight criterion for atoms in the view") Float maxWeight,
                                           @ExtensionRequestParameter(name = "defaultWeight", description = "weight of new atoms added to the view") Float defaultWeight,
                                           @ExtensionRequestParameter(name = "minSharability", description = "minimum-sharability criterion for atoms in the view") Float minSharability,
                                           @ExtensionRequestParameter(name = "maxSharability", description = "maximum-sharability criterion for atoms in the view") Float maxSharability,
                                           @ExtensionRequestParameter(name = "defaultSharability", description = "sharability of new atoms added to the view") Float defaultSharability,
                                           @ExtensionRequestParameter(name = "view", description = "the updated view") String view,
                                           @ExtensionRequestParameter(name = "style", description = "the style of view to generate") String styleName) {

        logInfo("tinkernotes update " + rootId + " (depth " + depth + ")");

        Params p = createParams(context, (KeyIndexableGraph) graph);
        p.depth = depth;
        p.rootId = rootId;
        p.styleName = styleName;
        p.view = view;

        // TODO: remove these; they're only for debugging a Rexster issue
        /*
        if (minWeight > 1000) minWeight = 1000f;
        if (maxWeight > 1000) maxWeight = 1000f;
        if (defaultWeight > 1000) defaultWeight = 1000f;
        if (minSharability > 1000) minSharability = 1000f;
        if (maxSharability > 1000) maxSharability = 1000f;
        if (defaultSharability > 1000) defaultSharability = 1000f;
        */

        p.filter = createFilter(p.user, minWeight, maxWeight, defaultWeight, minSharability, maxSharability, defaultSharability);

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        Note rootNote;

        InputStream in = new ByteArrayInputStream(p.view.getBytes());
        try {
            rootNote = p.parser.parse(in);
        } finally {
            in.close();
        }

        // Apply the update
        try {
            p.queries.update(p.root, rootNote, p.depth, p.filter, p.style, p.graph.getActivityLog());
        } catch (NoteQueries.InvalidUpdateException e) {
            return ExtensionResponse.error("invalid update: " + e.getMessage());
        }

        Note n = p.queries.view(p.root, p.depth, p.filter, p.style, p.graph.getActivityLog());
        addView(n, p);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return true;
    }
}
