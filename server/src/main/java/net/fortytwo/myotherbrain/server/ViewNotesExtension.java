package net.fortytwo.myotherbrain.server;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.frames.FramesManager;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.AbstractRexsterExtension;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.myotherbrain.notes.Note;
import net.fortytwo.myotherbrain.notes.NotesIO;
import net.fortytwo.myotherbrain.notes.NotesViews;

import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * User: josh
 * Date: 6/19/11
 * Time: 1:40 PM
 */
@ExtensionNaming(namespace = "myotherbrain", name = "view-notes")
public class ViewNotesExtension extends AbstractRexsterExtension {
    private static final Logger LOGGER = Logger.getLogger(ViewNotesExtension.class.getName());

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for viewing a portion of a MyOtherBrain graph in the MOB Notes format")
    public ExtensionResponse handleViewRequest(@RexsterContext RexsterResourceContext context,
                                               @RexsterContext Graph graph,
                                               @ExtensionRequestParameter(name = "root", description = "root atom (vertex) of the view") String root) {
        try {
            LOGGER.fine("view-notes request for: " + root);
            int depth = 3;

            if (!NotesIO.KEY.matcher(root).matches()) {
                return ExtensionResponse.error("root of view is not a valid key: " + root);
            }

            if (!(graph instanceof IndexableGraph)) {
                return ExtensionResponse.error("graph must be an instance of IndexableGraph");
            }

            Map<String, String> map = new HashMap<String, String>();
            map.put("root", root);
            map.put("depth", "" + depth);

            FramesManager manager = new FramesManager(graph);
            NotesViews m = new NotesViews((IndexableGraph) graph, manager);
            NotesIO p = new NotesIO();

            Note n = m.toNote(root, depth);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            try {
                p.writeChildren(n, bos);
                map.put("view", bos.toString());
            } finally {
                bos.close();
            }

            return ExtensionResponse.ok(map);
        } catch (Exception e) {
            // TODO
            e.printStackTrace(System.out);
            return ExtensionResponse.error(e);
        }
    }
}
