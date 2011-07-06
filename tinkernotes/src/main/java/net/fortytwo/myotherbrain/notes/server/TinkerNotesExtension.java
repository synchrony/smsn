package net.fortytwo.myotherbrain.notes.server;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.blueprints.pgm.TransactionalGraph;
import com.tinkerpop.frames.FramesManager;
import com.tinkerpop.rexster.extension.AbstractRexsterExtension;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.notes.Filter;
import net.fortytwo.myotherbrain.notes.Note;
import net.fortytwo.myotherbrain.notes.NotesSemantics;
import net.fortytwo.myotherbrain.notes.NotesSyntax;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class TinkerNotesExtension extends AbstractRexsterExtension {
    protected static final Logger LOGGER = Logger.getLogger(TinkerNotesExtension.class.getName());

    protected ExtensionResponse handleRequestInternal(final Params p,
                                                      final String rootKey) {
        try {
            p.map = new HashMap<String, String>();

            if (!(p.graph instanceof IndexableGraph)) {
                return ExtensionResponse.error("graph must be an instance of IndexableGraph");
            }

            if (null != p.view) {
                // Force the use of the UTF-8 charset, which is apparently not chosen by Jersey
                // even when it is specified by the client in the Content-Type header, e.g.
                //    Content-Type: application/x-www-form-urlencoded;charset=UTF-8
                p.view = new String(p.view.getBytes("UTF-8"));
            }

            p.manager = new FramesManager(p.graph);
            p.m = new NotesSemantics((IndexableGraph) p.graph, p.manager);
            p.syntax = new NotesSyntax();

            if (null != p.depth) {
                if (p.depth < 1) {
                    return ExtensionResponse.error("depth must be at least 1");
                }

                if (p.depth > 5) {
                    return ExtensionResponse.error("depth may not be more than 5");
                }

                p.map.put("depth", "" + p.depth);
            }

            if (null != p.filter) {
                if (p.filter.minSharability < 0 || p.filter.maxSharability > 1) {
                    return ExtensionResponse.error("minimum and maximum sharability must lie between 0 and 1 (inclusive)");
                }

                if (p.filter.maxSharability < p.filter.minSharability) {
                    return ExtensionResponse.error("maximum sharability must be greater than or equal to minimum sharability");
                }

                p.map.put("minSharability", "" + p.filter.minSharability);
                p.map.put("maxSharability", "" + p.filter.maxSharability);

                if (p.filter.minWeight < 0 || p.filter.maxWeight > 1) {
                    return ExtensionResponse.error("minimum and maximum weight must lie between 0 and 1 (inclusive)");
                }

                if (p.filter.maxWeight < p.filter.minWeight) {
                    return ExtensionResponse.error("maximum weight must be greater than or equal to minimum weight");
                }

                p.map.put("minWeight", "" + p.filter.minWeight);
                p.map.put("maxWeight", "" + p.filter.maxWeight);
            }

            if (null != rootKey) {
                p.root = p.m.getAtom(rootKey);
                if (null == p.root || (null != p.filter && !p.filter.isVisible(p.root))) {
                    return ExtensionResponse.error("root of view does not exist or is not visible: " + rootKey);
                }

                p.map.put("root", rootKey);
                p.map.put("title", null == p.root.getValue() || 0 == p.root.getValue().length() ? "[no title]" : p.root.getValue());
            }

            if (null != p.inverse) {
                p.map.put("inverse", "" + p.inverse);
            }

            boolean manual = p.graph instanceof TransactionalGraph
                    && TransactionalGraph.Mode.MANUAL == ((TransactionalGraph) p.graph).getTransactionMode();

            if (manual) {
                ((TransactionalGraph) p.graph).startTransaction();
            }
            boolean normal = false;

            try {
                ExtensionResponse r = performTransaction(p);
                normal = true;
                return r;
            } finally {
                if (manual) {
                    if (!normal) {
                        System.err.println("rolling back transaction");
                    }

                    ((TransactionalGraph) p.graph).stopTransaction(normal
                            ? TransactionalGraph.Conclusion.SUCCESS
                            : TransactionalGraph.Conclusion.FAILURE);
                }
            }
        } catch (Exception e) {
            // TODO
            e.printStackTrace(System.out);
            return ExtensionResponse.error(e);
        }
    }

    protected void addView(final Params p) throws IOException {
        Note n = p.m.view(p.root, p.depth, p.filter, p.inverse);
        JSONObject json;

        try {
            json = p.syntax.toJSON(n);
        } catch (JSONException e) {
            throw new IOException(e);
        }
        p.map.put("view", json.toString());
    }

    protected abstract ExtensionResponse performTransaction(Params p) throws Exception;

    protected class Params {
        public Map<String, String> map;
        public Graph graph;
        public FramesManager manager;
        public NotesSemantics m;
        public NotesSyntax syntax;
        public Atom root;
        public Integer depth;
        public String view;
        public Boolean inverse;
        public Filter filter;
        public String query;
        public Float newWeight;
        public Float newSharability;
    }
}
