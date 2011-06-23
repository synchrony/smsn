package net.fortytwo.myotherbrain.notes.server;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.frames.FramesManager;
import com.tinkerpop.rexster.extension.AbstractRexsterExtension;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.notes.Filter;
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
                                                      final Filter filter,
                                                      final String view,
                                                      final boolean inverse) {
        try {
            if (depth < 1) {
                return ExtensionResponse.error("depth must be at least 1");
            }

            if (depth > 5) {
                return ExtensionResponse.error("depth may not be more than 5");
            }

            if (filter.minVisibility < 0 || filter.maxVisibility > 1) {
                return ExtensionResponse.error("minimum and maximum visibility must lie between 0 and 1 (inclusive)");
            }

            if (filter.maxVisibility < filter.minVisibility) {
                return ExtensionResponse.error("maximum visibility must be greater than or equal to minimum visibility");
            }

            if (filter.minWeight < 0 || filter.maxWeight > 1) {
                return ExtensionResponse.error("minimum and maximum weight must lie between 0 and 1 (inclusive)");
            }

            if (filter.maxWeight < filter.minWeight) {
                return ExtensionResponse.error("maximum weight must be greater than or equal to minimum weight");
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
            map.put("inverse", "" + inverse);
            map.put("minVisibility", "" + filter.minVisibility);
            map.put("maxVisibility", "" + filter.maxVisibility);
            map.put("minWeight", "" + filter.minWeight);
            map.put("maxWeight", "" + filter.maxWeight);

            FramesManager manager = new FramesManager(graph);
            NotesSemantics m = new NotesSemantics((IndexableGraph) graph, manager);
            NotesSyntax syntax = new NotesSyntax();

            Atom root = m.getAtom(rootKey);
            if (null == root) {
                return ExtensionResponse.error("no such atom: " + rootKey);
            }
            map.put("title", null == root.getValue() || 0 == root.getValue().length() ? "[no name]" : root.getValue());

            Params p = new Params();
            p.map = map;
            p.graph = graph;
            p.manager = manager;
            p.m = m;
            p.p = syntax;
            p.root = root;
            p.depth = depth;
            p.view = view;
            p.inverse = inverse;
            p.filter = filter;
            return handleRequestProtected(p);
        } catch (Exception e) {
            // TODO
            e.printStackTrace(System.out);
            return ExtensionResponse.error(e);
        }
    }

    protected void addView(final Params p) throws IOException {
        Note n = p.m.view(p.root, p.depth, p.filter, p.inverse);

        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        try {
            p.p.writeChildren(n, bos);
            p.map.put("view", bos.toString());
        } finally {
            bos.close();
        }
    }

    protected abstract ExtensionResponse handleRequestProtected(Params p) throws Exception;


    protected class Params {
        public Map<String, String> map;
        public Graph graph;
        public FramesManager manager;
        public NotesSemantics m;
        public NotesSyntax p;
        public Atom root;
        public int depth;
        public String view;
        public boolean inverse;
        public Filter filter;
    }
}
