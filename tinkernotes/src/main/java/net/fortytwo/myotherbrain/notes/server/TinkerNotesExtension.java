package net.fortytwo.myotherbrain.notes.server;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.frames.FramesManager;
import com.tinkerpop.rexster.extension.AbstractRexsterExtension;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import net.fortytwo.myotherbrain.model.frames.Atom;
import net.fortytwo.myotherbrain.notes.Note;
import net.fortytwo.myotherbrain.notes.NotesSemantics;
import net.fortytwo.myotherbrain.notes.NotesSyntax;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * User: josh
 * Date: 6/19/11
 * Time: 1:40 PM
 */
public abstract class TinkerNotesExtension extends AbstractRexsterExtension {
    protected static final Logger LOGGER = Logger.getLogger(TinkerNotesExtension.class.getName());

    protected ExtensionResponse handleRequestInternal(final Graph graph,
                                                      final String rootKey,
                                                      final int depth,
                                                      final String view) {
        try {
            if (depth < 1) {
                return ExtensionResponse.error("depth must be at least 1");
            }

            if (depth > 5) {
                return ExtensionResponse.error("depth may not be more than 5");
            }

            if (!NotesSyntax.KEY.matcher(rootKey).matches()) {
                return ExtensionResponse.error("root of view is not a valid key: " + rootKey);
            }

            if (!(graph instanceof IndexableGraph)) {
                return ExtensionResponse.error("graph must be an instance of IndexableGraph");
            }

            Map<String, String> map = new HashMap<String, String>();
            map.put("root", rootKey);
            map.put("depth", "" + depth);

            FramesManager manager = new FramesManager(graph);
            NotesSemantics m = new NotesSemantics((IndexableGraph) graph, manager);
            NotesSyntax p = new NotesSyntax();

            Atom root = m.getAtom(rootKey);
            if (null == root) {
                return ExtensionResponse.error("no such atom: " + rootKey);
            }
            map.put("title", null == root.getText() || 0 == root.getText().length() ? "[no name]" : root.getText());

            return handleRequestProtected(map, graph, manager, m, p, root, depth, view);
        } catch (Exception e) {
            // TODO
            e.printStackTrace(System.out);
            return ExtensionResponse.error(e);
        }
    }

    protected void addView(final Map<String, String> map,
                           final NotesSemantics m,
                           final Atom root,
                           final int depth,
                           final NotesSyntax p) throws IOException {
        Note n = m.view(root, depth);

        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        try {
            p.writeChildren(n, bos);
            map.put("view", bos.toString());
        } finally {
            bos.close();
        }
    }

    protected abstract ExtensionResponse handleRequestProtected(final Map<String, String> map,
                                                                final Graph graph,
                                                                final FramesManager manager,
                                                                final NotesSemantics m,
                                                                final NotesSyntax p,
                                                                final Atom root,
                                                                final int depth,
                                                                final String view) throws Exception;
}
