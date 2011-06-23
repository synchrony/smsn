package net.fortytwo.myotherbrain.notes.server;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.frames.FramesManager;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.myotherbrain.model.frames.Atom;
import net.fortytwo.myotherbrain.notes.NotesSyntax;
import net.fortytwo.myotherbrain.notes.NotesSemantics;

import java.util.Map;

/**
 * User: josh
 * Date: 6/19/11
 * Time: 1:40 PM
 */
@ExtensionNaming(namespace = "myotherbrain", name = "view-notes")
public class ViewExtension extends TinkerNotesExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for viewing a portion of a MyOtherBrain graph in the TinkerNotes format")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "root", description = "root atom (vertex) of the view") String rootKey,
                                           @ExtensionRequestParameter(name = "depth", description = "depth of the view") Integer depth) {
        LOGGER.fine("view-notes request for: " + rootKey);

        return this.handleRequestInternal(graph, rootKey, depth, null);
    }

    @Override
    protected ExtensionResponse handleRequestProtected(final Map<String, String> map,
                                                       final Graph graph,
                                                       final FramesManager manager,
                                                       final NotesSemantics m,
                                                       final NotesSyntax p,
                                                       final Atom root,
                                                       final int depth,
                                                       final String view) throws Exception {
        addView(map, m, root, depth, p);

        return ExtensionResponse.ok(map);
    }
}
