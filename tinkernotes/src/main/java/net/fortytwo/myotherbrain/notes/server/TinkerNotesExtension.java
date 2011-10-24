package net.fortytwo.myotherbrain.notes.server;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.blueprints.pgm.TransactionalGraph;
import com.tinkerpop.frames.FramesManager;
import com.tinkerpop.rexster.extension.AbstractRexsterExtension;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.MOBGraph;
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

    protected abstract ExtensionResponse performTransaction(Params p) throws Exception;

    protected abstract boolean isReadOnly();

    protected ExtensionResponse handleRequestInternal(final Params p) {
        String rootKey = p.rootKey;
        String styleName = p.styleName;

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
            MOBGraph store = new MOBGraph((IndexableGraph) p.graph);
            p.semantics = new NotesSemantics(store, p.manager);
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
                p.map.put("minSharability", "" + p.filter.minSharability);
                p.map.put("maxSharability", "" + p.filter.maxSharability);
                p.map.put("defaultSharability", "" + p.filter.defaultSharability);
                p.map.put("minWeight", "" + p.filter.minWeight);
                p.map.put("maxWeight", "" + p.filter.maxWeight);
                p.map.put("defaultWeight", "" + p.filter.defaultWeight);
            }

            if (null != rootKey) {
                p.root = p.semantics.getAtom(rootKey);
                if (null == p.root || (null != p.filter && !p.filter.isVisible(p.root))) {
                    return ExtensionResponse.error("root of view does not exist or is not visible: " + rootKey);
                }

                p.map.put("root", rootKey);
                p.map.put("title", null == p.root.getValue() || 0 == p.root.getValue().length() ? "[no title]" : p.root.getValue());
            }

            if (null != styleName) {
                p.style = NotesSemantics.ViewStyle.find(styleName);

                if (null == p.style) {
                    return ExtensionResponse.error("unsupported view style: " + styleName);
                }

                p.map.put("style", p.style.getName());
            }

            boolean manual;
            // Force manual transaction mode (provided that the graph is transactional)
            if (!isReadOnly() && p.graph instanceof TransactionalGraph) {
                if (0 >= ((TransactionalGraph) p.graph).getCurrentBufferSize()) {
                    ((TransactionalGraph) p.graph).setMaxBufferSize(-1);
                }
                manual = true;

                ((TransactionalGraph) p.graph).startTransaction();
            } else {
                manual = false;
            }

            //System.err.println("transactional: " + (p.graph instanceof TransactionalGraph));
            //System.err.println("mode: " + ((TransactionalGraph) p.graph).getTransactionMode());
            //System.err.println("graph: " + p.graph);
            //System.err.println("class: " + p.graph.getClass());

            boolean normal = false;

            try {
                ExtensionResponse r = performTransaction(p);
                normal = true;
                return r;
            } finally {
                if (!isReadOnly()) {
                    if (manual) {
                        if (!normal) {
                            System.err.println("rolling back transaction");
                        }

                        ((TransactionalGraph) p.graph).stopTransaction(normal
                                ? TransactionalGraph.Conclusion.SUCCESS
                                : TransactionalGraph.Conclusion.FAILURE);
                    } else if (!normal) {
                        System.err.println("failed update of non-transactional graph. Data integrity is not guaranteed");
                    }
                }
            }
        } catch (Exception e) {
            // TODO
            e.printStackTrace(System.err);
            LOGGER.warning("operation failed: " + e.getMessage());
            return ExtensionResponse.error(e);
        }
    }

    protected void addView(final Params p) throws IOException {
        Note n = p.semantics.view(p.root, p.depth, p.filter, p.style);
        JSONObject json;

        try {
            json = p.syntax.toJSON(n);
        } catch (JSONException e) {
            throw new IOException(e);
        }
        p.map.put("view", json.toString());
    }

    protected class Params {
        public Map<String, String> map;
        public Graph graph;
        public FramesManager manager;
        public NotesSemantics semantics;
        public NotesSyntax syntax;
        public Atom root;
        public Integer depth;
        public String view;
        public NotesSemantics.ViewStyle style;
        public Filter filter;
        public String query;
        public Float newWeight;
        public Float newSharability;
        public String rootKey;
        public String styleName;
        public String file;
    }
}
