package net.fortytwo.extendo.server;

import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.TransactionalGraph;
import com.tinkerpop.frames.FramedGraph;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.AbstractRexsterExtension;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.BrainGraph;
import net.fortytwo.extendo.brain.ExtendoBrain;
import net.fortytwo.extendo.brain.Filter;
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.extendo.brain.NoteHistory;
import net.fortytwo.extendo.brain.NoteQueries;
import net.fortytwo.extendo.brain.wiki.NoteParser;
import net.fortytwo.extendo.brain.wiki.NoteWriter;
import org.json.JSONException;
import org.json.JSONObject;

import javax.servlet.http.HttpSession;
import javax.ws.rs.core.SecurityContext;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.Principal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class ExtendoExtension extends AbstractRexsterExtension {
    protected static final Logger LOGGER = Logger.getLogger(ExtendoExtension.class.getName());

    private static final String HISTORY_ATTR = "history";

    protected abstract ExtensionResponse performTransaction(Params p) throws Exception;

    protected abstract boolean doesRead();

    protected abstract boolean doesWrite();

    private static final Map<KeyIndexableGraph, ExtendoBrain> brains = new HashMap<KeyIndexableGraph, ExtendoBrain>();

    public synchronized static ExtendoBrain getBrain(final KeyIndexableGraph baseGraph) throws ExtendoBrain.ExtendoBrainException {
        ExtendoBrain b = brains.get(baseGraph);

        if (null == b) {
            BrainGraph bg = new BrainGraph(baseGraph);
            b = new ExtendoBrain(bg);
            brains.put(baseGraph, b);
        }

        return b;
    }

    protected Params createParams(final RexsterResourceContext context,
                                  final KeyIndexableGraph graph) {
        Params p = new Params();
        p.baseGraph = graph;
        p.context = context;
        SecurityContext security = p.context.getSecurityContext();
        p.user = null == security ? null : security.getUserPrincipal();

        // TODO: reconsider security
        if (null == p.user) {
            //    logWarning("no security");
        }

        return p;
    }

    protected ExtensionResponse handleRequestInternal(final Params p) {

        if (doesWrite() && !canWrite(p.user)) {
            return ExtensionResponse.error("user does not have permission to for write operations");
        }

        if (doesRead() && null == p.filter) {
            return ExtensionResponse.error("service reads from graph, but weight and sharability filter is not set");
        }

        String rootKey = p.rootId;
        String styleName = p.styleName;

        try {
            p.map = new HashMap<String, Object>();

            if (!(p.baseGraph instanceof KeyIndexableGraph)) {
                return ExtensionResponse.error("graph must be an instance of IndexableGraph");
            }

            if (null != p.wikiView) {
                // Force the use of the UTF-8 charset, which is apparently not chosen by Jersey
                // even when it is specified by the client in the Content-Type header, e.g.
                //    Content-Type: application/x-www-form-urlencoded;charset=UTF-8
                p.wikiView = new String(p.wikiView.getBytes("UTF-8"));
            }

            p.manager = new FramedGraph<KeyIndexableGraph>(p.baseGraph);
            p.brain = getBrain(p.baseGraph);
            p.queries = new NoteQueries(p.brain);
            p.parser = new NoteParser();
            p.writer = new NoteWriter();

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
                p.map.put("minSharability", "" + p.filter.getMinSharability());
                p.map.put("maxSharability", "" + p.filter.getMaxSharability());
                p.map.put("defaultSharability", "" + p.filter.getDefaultSharability());
                p.map.put("minWeight", "" + p.filter.getMinWeight());
                p.map.put("maxWeight", "" + p.filter.getMaxWeight());
                p.map.put("defaultWeight", "" + p.filter.getDefaultWeight());
            }

            if (null != rootKey) {
                p.root = p.brain.getBrainGraph().getAtom(rootKey);

                if (null == p.root) {
                    return ExtensionResponse.error("root of view does not exist: " + rootKey);
                }

                if (null != p.filter && !p.filter.isVisible(p.root)) {
                    return ExtensionResponse.error("root of view is not visible: " + rootKey);
                }

                p.map.put("root", rootKey);
            }

            p.map.put("title", null == p.root || null == p.root.getValue() || 0 == p.root.getValue().length() ? "[no title]" : p.root.getValue());

            if (null != styleName) {
                p.style = NoteQueries.lookupStyle(styleName);
                p.map.put("style", p.style.getName());
            }

            // Force manual transaction mode (provided that the graph is transactional)
            boolean manual = doesWrite() && p.baseGraph instanceof TransactionalGraph;

            boolean normal = false;

            try {
                ExtensionResponse r = performTransaction(p);
                normal = true;

                // Note: currently, all activities are logged, but the log is not immediately flushed
                //       unless the transaction succeeds.
                if (null != p.brain.getActivityLog()) {
                    p.brain.getActivityLog().flush();
                }

                return r;
            } finally {
                if (doesWrite()) {
                    if (manual) {
                        if (!normal) {
                            logWarning("rolling back transaction");
                        }

                        ((TransactionalGraph) p.baseGraph).stopTransaction(normal
                                ? TransactionalGraph.Conclusion.SUCCESS
                                : TransactionalGraph.Conclusion.FAILURE);
                    } else if (!normal) {
                        logWarning("failed update of non-transactional graph. Data integrity is not guaranteed");
                    }
                }
            }
        } catch (Exception e) {
            logWarning("operation failed with exception: " + e.getMessage());
            // TODO
            e.printStackTrace(System.err);
            return ExtensionResponse.error(e);
        } catch (Throwable t) {
            logWarning("operation failed with throwable: " + t.getMessage());
            // TODO
            t.printStackTrace(System.err);
            return ExtensionResponse.error(t.getMessage());
        }
    }

    protected Filter createFilter(final Principal user,
                                  final float minWeight,
                                  final float maxWeight,
                                  final float defaultWeight,
                                  final float minSharability,
                                  final float maxSharability,
                                  final float defaultSharability) {

        float m = findMinAuthorizedSharability(user, minSharability);
        return new Filter(minWeight, maxWeight, defaultWeight, m, maxSharability, defaultSharability);
    }

    protected org.codehaus.jettison.json.JSONObject toJettison(JSONObject j) throws IOException {
        try {
            return new org.codehaus.jettison.json.JSONObject(j.toString());
        } catch (org.codehaus.jettison.json.JSONException e) {
            throw new IOException(e);
        }
    }

    protected void addView(final Note n,
                           final Params p) throws IOException {
        JSONObject json;

        try {
            json = p.writer.toJSON(n);
        } catch (JSONException e) {
            throw new IOException(e);
        }

        p.map.put("view", toJettison(json));
    }

    public static float findMinAuthorizedSharability(final Principal user,
                                                 final float minSharability) {
        // TODO
        float minAuth = (null == user)
                ? 0.0f
                : !user.getName().equals("josh")
                ? 0.75f : 0;

        return Math.max(minSharability, minAuth);
    }

    protected boolean canWrite(final Principal user) {
        // TODO
        return null == user || user.getName().equals("josh");
    }

    private NoteHistory getNotesHistory(final RexsterResourceContext context) {
        HttpSession session = context.getRequest().getSession();
        NoteHistory h = (NoteHistory) session.getAttribute(HISTORY_ATTR);
        if (null == h) {
            h = new NoteHistory();
            session.setAttribute(HISTORY_ATTR, h);
        }

        return h;
    }

    protected void addToHistory(final String rootId,
                                final RexsterResourceContext context) {
        NoteHistory h = getNotesHistory(context);
        h.visit(rootId);
    }

    protected List<String> getHistory(final RexsterResourceContext context,
                                      final BrainGraph graph,
                                      final Filter filter) {
        NoteHistory h = getNotesHistory(context);
        return h.getHistory(100, true, graph, filter);
    }

    protected void logInfo(final String message) {
        LOGGER.info(message);
        //System.err.println(message);
    }

    protected void logWarning(final String message) {
        LOGGER.warning(message);
        //System.err.println(message);
    }

    protected class Params {
        public KeyIndexableGraph baseGraph;
        public ExtendoBrain brain;
        public RexsterResourceContext context;
        public String data;
        public Integer depth;
        public String file;
        public Filter filter;
        public String format;
        public boolean includeTypes;
        public JSONObject jsonView;
        public FramedGraph<KeyIndexableGraph> manager;
        public Map<String, Object> map;
        public Integer maxResults;
        public NoteParser parser;
        public String propertyName;
        public Float propertyValue;
        public NoteQueries queries;
        public String query;
        public Atom root;
        public String rootId;
        public NoteQueries.AdjacencyStyle style;
        public String styleName;
        public Principal user;
        public Integer valueCutoff;
        public String wikiView;
        public NoteWriter writer;
    }

}
