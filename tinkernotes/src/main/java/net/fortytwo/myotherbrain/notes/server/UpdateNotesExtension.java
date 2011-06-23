package net.fortytwo.myotherbrain.notes.server;

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
import com.tinkerpop.rexster.extension.HttpMethod;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.myotherbrain.model.frames.Atom;
import net.fortytwo.myotherbrain.notes.Note;
import net.fortytwo.myotherbrain.notes.NotesIO;
import net.fortytwo.myotherbrain.notes.NotesLens;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * User: josh
 * Date: 6/19/11
 * Time: 1:40 PM
 */
@ExtensionNaming(namespace = "myotherbrain", name = "update-notes")
public class UpdateNotesExtension extends AbstractRexsterExtension {
    private static final Logger LOGGER = Logger.getLogger(UpdateNotesExtension.class.getName());

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH, method = HttpMethod.POST)
    @ExtensionDescriptor(description = "an extension for updating a portion of a MyOtherBrain graph using the MOB Notes format")
    public ExtensionResponse handleUpdateRequest(@RexsterContext RexsterResourceContext context,
                                                 @RexsterContext Graph graph,
                                                 @ExtensionRequestParameter(name = "root", description = "root atom (vertex) of the view") String rootKey,
                                                 @ExtensionRequestParameter(name = "view", description = "the updated view") String view) {
        //new Exception().printStackTrace(System.out);
        try {
            LOGGER.fine("update-notes request for: " + rootKey);
            System.out.println("update-notes request for: " + rootKey);
            int depth = 3;

            if (!NotesIO.KEY.matcher(rootKey).matches()) {
                return ExtensionResponse.error("root of view is not a valid key: " + rootKey);
            }

            if (!(graph instanceof IndexableGraph)) {
                return ExtensionResponse.error("graph must be an instance of IndexableGraph");
            }

            Map<String, String> map = new HashMap<String, String>();
            map.put("root", rootKey);
            map.put("depth", "" + depth);

            FramesManager manager = new FramesManager(graph);
            NotesLens m = new NotesLens((IndexableGraph) graph, manager);
            NotesIO p = new NotesIO();

            List<Note> children;

            InputStream in = new ByteArrayInputStream(view.getBytes());
            try {
                children = p.parseNotes(in);
            } finally {
                in.close();
            }

            Atom root = m.getAtom(rootKey);
            if (null == root) {
                return ExtensionResponse.error("no such atom: " + rootKey);
            }

            // Apply the update
            try {
                // TODO: pass the root node to update(), and use (depth) instead of (depth-1)
                m.update(root, children, depth - 1);
            } catch (NotesLens.InvalidUpdateException e) {
                return ExtensionResponse.error("invalid update: " + e.getMessage());
            }

            // Finally, generate a fresh view (post-update) and return it to the requester.
            Note n = m.view(m.getAtom(rootKey), depth);
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
