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
import com.tinkerpop.rexster.extension.HttpMethod;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.notes.Note;
import net.fortytwo.myotherbrain.notes.NotesSemantics;
import net.fortytwo.myotherbrain.notes.NotesSyntax;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.List;
import java.util.Map;

/**
 * User: josh
 * Date: 6/19/11
 * Time: 1:40 PM
 */
@ExtensionNaming(namespace = "myotherbrain", name = "update-notes")
public class UpdateExtension extends TinkerNotesExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH, method = HttpMethod.POST)
    @ExtensionDescriptor(description = "an extension for updating a portion of a MyOtherBrain graph using the MOB Notes format")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "root", description = "root atom (vertex) of the view") String rootKey,
                                           @ExtensionRequestParameter(name = "depth", description = "depth of the view") Integer depth,
                                           @ExtensionRequestParameter(name = "view", description = "the updated view") String view,
                                           @ExtensionRequestParameter(name = "inverse", description = "whether to create an inverted view") Boolean inverse) {

        LOGGER.fine("update-notes request for: " + rootKey);
        System.out.println("update-notes request for: " + rootKey);

        return this.handleRequestInternal(graph, rootKey, depth, view, inverse);
    }

    @Override
    protected ExtensionResponse handleRequestProtected(final Map<String, String> map,
                                                       final Graph graph,
                                                       final FramesManager manager,
                                                       final NotesSemantics m,
                                                       final NotesSyntax p,
                                                       final Atom root,
                                                       final int depth,
                                                       final String view,
                                                       final boolean inverse) throws Exception {
        List<Note> children;

        InputStream in = new ByteArrayInputStream(view.getBytes());
        try {
            children = p.parseNotes(in);
        } finally {
            in.close();
        }

        // Apply the update
        try {
            m.update(root, children, depth, inverse);
        } catch (NotesSemantics.InvalidUpdateException e) {
            return ExtensionResponse.error("invalid update: " + e.getMessage());
        }

        addView(map, m, root, depth, p, inverse);

        return ExtensionResponse.ok(map);
    }
}
