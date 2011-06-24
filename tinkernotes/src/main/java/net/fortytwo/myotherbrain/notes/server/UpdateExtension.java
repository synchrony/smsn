package net.fortytwo.myotherbrain.notes.server;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.HttpMethod;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.myotherbrain.notes.Filter;
import net.fortytwo.myotherbrain.notes.Note;
import net.fortytwo.myotherbrain.notes.NotesSemantics;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.List;

/**
 * User: josh
 * Date: 6/19/11
 * Time: 1:40 PM
 */
@ExtensionNaming(namespace = "tinkernotes", name = "update")
public class UpdateExtension extends TinkerNotesExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH, method = HttpMethod.POST)
    @ExtensionDescriptor(description = "an extension for updating a portion of a MyOtherBrain graph using the MOB Notes format")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "root", description = "root atom (vertex) of the view") String rootKey,
                                           @ExtensionRequestParameter(name = "depth", description = "depth of the view") Integer depth,
                                           @ExtensionRequestParameter(name = "minWeight", description = "minimum-weight criterion for atoms in the view") Float minWeight,
                                           @ExtensionRequestParameter(name = "maxWeight", description = "maximum-weight criterion for atoms in the view") Float maxWeight,
                                           @ExtensionRequestParameter(name = "minSharability", description = "minimum-sharability criterion for atoms in the view") Float minSharability,
                                           @ExtensionRequestParameter(name = "maxSharability", description = "maximum-sharability criterion for atoms in the view") Float maxSharability,
                                           @ExtensionRequestParameter(name = "view", description = "the updated view") String view,
                                           @ExtensionRequestParameter(name = "inverse", description = "whether to create an inverted view") Boolean inverse) {

        LOGGER.fine("update-notes request for: " + rootKey);
        System.out.println("update-notes request for: " + rootKey);

        Filter filter = new Filter(minSharability, maxSharability, minWeight, maxWeight);

        return this.handleRequestInternal(graph, rootKey, depth, filter, view, inverse);
    }

    @Override
    protected ExtensionResponse handleRequestProtected(final Params p) throws Exception {
        List<Note> children;

        InputStream in = new ByteArrayInputStream(p.view.getBytes());
        try {
            children = p.p.parseNotes(in);
        } finally {
            in.close();
        }

        // Apply the update
        try {
            p.m.update(p.root, children, p.depth, p.filter, p.inverse);
        } catch (NotesSemantics.InvalidUpdateException e) {
            return ExtensionResponse.error("invalid update: " + e.getMessage());
        }

        addView(p);

        return ExtensionResponse.ok(p.map);
    }
}
