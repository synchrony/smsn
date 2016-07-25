package net.fortytwo.smsn.server;

import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.TransactionalGraph;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.NoteHistory;
import net.fortytwo.smsn.brain.NoteQueries;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;
import net.fortytwo.smsn.brain.wiki.NoteParser;
import net.fortytwo.smsn.brain.wiki.NoteWriter;
import net.fortytwo.smsn.server.error.AuthorizationException;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

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
public abstract class Action {
    protected static final Logger logger = Logger.getLogger(Action.class.getName());

    public abstract String getName();

    private static final int MAX_VIEW_HEIGHT = 7;

    protected abstract void performTransaction(RequestParams p) throws BadRequestException, RequestProcessingException;

    protected abstract boolean doesRead();

    protected abstract boolean doesWrite();

    private static final Map<KeyIndexableGraph, Brain> brains = new HashMap<>();

    private static final NoteHistory noteHistory = new NoteHistory();

    private synchronized static Brain getBrain(final KeyIndexableGraph baseGraph)
            throws Brain.BrainException {

        Brain b = brains.get(baseGraph);

        if (null == b) {
            logger.info("instantiating Extend-o-Brain with base graph " + baseGraph);
            AtomGraph bg = new PGAtomGraph(baseGraph);
            b = new Brain(bg);
            b.startBackgroundTasks();
            brains.put(baseGraph, b);
        }

        return b;
    }

    public abstract void parseRequest(final JSONObject request, final RequestParams p) throws JSONException, BadRequestException;

    public void parseRequest(final String request, final RequestParams p) throws JSONException, BadRequestException {
        parseRequest(new JSONObject(request), p);
    }

    public static RequestParams createParams(final KeyIndexableGraph graph) {
        RequestParams p = new RequestParams();
        p.baseGraph = graph;
        p.user = () -> "none";

        return p;
    }

    private void checkAuthorized(final RequestParams p) throws AuthorizationException {
        if (doesWrite() && !canWrite(p.user)) {
            throw new AuthorizationException("user does not have permission to for write operations");
        }

        if (doesRead() && null == p.filter) {
            throw new AuthorizationException("service reads from graph, but weight and sharability filter is not set");
        }
    }

    public void handleRequestInternal(final RequestParams p)
            throws AuthorizationException, BadRequestException, RequestProcessingException {

        checkAuthorized(p);

        String rootId = p.rootId;
        String styleName = p.styleName;

        p.map = new HashMap<>();

        if (null != p.wikiView) {
            // Force the use of the UTF-8 charset, which is apparently not chosen by Jersey
            // even when it is specified by the client in the Content-Type header, e.g.
            //    Content-Type: application/x-www-form-urlencoded;charset=UTF-8
            try {
                p.wikiView = new String(p.wikiView.getBytes("UTF-8"));
            } catch (UnsupportedEncodingException e) {
                throw new RequestProcessingException(e);
            }
        }

        try {
            p.brain = getBrain(p.baseGraph);
        } catch (Brain.BrainException e) {
            throw new RequestProcessingException(e);
        }
        p.queries = new NoteQueries(p.brain);
        p.parser = new NoteParser();
        p.writer = new NoteWriter();

        if (null != p.height) {
            if (p.height < 0) {
                throw new BadRequestException("height must be at least 0");
            }

            if (p.height > MAX_VIEW_HEIGHT) {
                throw new BadRequestException("height may not be more than 5");
            }

            p.map.put(Params.HEIGHT, "" + p.height);
        }

        if (null != p.filter) {
            p.map.put(Params.MIN_SHARABILITY, "" + p.filter.getMinSharability());
            p.map.put(Params.MAX_SHARABILITY, "" + p.filter.getMaxSharability());
            p.map.put(Params.DEFAULT_SHARABILITY, "" + p.filter.getDefaultSharability());
            p.map.put(Params.MIN_WEIGHT, "" + p.filter.getMinWeight());
            p.map.put(Params.MAX_WEIGHT, "" + p.filter.getMaxWeight());
            p.map.put(Params.DEFAULT_WEIGHT, "" + p.filter.getDefaultWeight());
        }

        if (null != rootId) {
            p.root = p.brain.getAtomGraph().getAtom(rootId);

            if (null == p.root) {
                throw new BadRequestException("root of view does not exist: " + rootId);
            }

            if (null != p.filter && !p.filter.isVisible(p.root)) {
                throw new BadRequestException("root of view is not visible: " + rootId);
            }

            p.map.put(Params.ROOT, rootId);
        }

        p.map.put(Params.TITLE, null == p.root
                || null == p.root.getValue()
                || 0 == p.root.getValue().length() ? "[no title]" : p.root.getValue());

        if (null != styleName) {
            p.style = NoteQueries.lookupStyle(styleName);
            p.map.put(Params.STYLE, p.style.getName());
        }

        // Force manual transaction mode (provided that the graph is transactional)
        boolean manual = doesWrite() && p.baseGraph instanceof TransactionalGraph;

        boolean normal = false;

        try {
            performTransaction(p);

            normal = true;

            // Note: currently, all activities are logged, but the log is not immediately flushed
            //       unless the transaction succeeds.
            if (null != p.brain.getActivityLog()) {
                p.brain.getActivityLog().flush();
            }
        } finally {
            if (doesWrite()) {
                if (manual) {
                    if (!normal) {
                        SemanticSynchrony.logWarning("rolling back transaction");
                    }

                    ((TransactionalGraph) p.baseGraph).stopTransaction(normal
                            ? TransactionalGraph.Conclusion.SUCCESS
                            : TransactionalGraph.Conclusion.FAILURE);
                } else if (!normal) {
                    SemanticSynchrony.logWarning(
                            "failed update of non-transactional graph. Inconsistent data is possible.");
                }
            }
        }
    }

    protected org.codehaus.jettison.json.JSONObject toJettison(JSONObject j) throws IOException {
        try {
            return new org.codehaus.jettison.json.JSONObject(j.toString());
        } catch (org.codehaus.jettison.json.JSONException e) {
            throw new IOException(e);
        }
    }

    protected void addView(final Note n,
                           final RequestParams p) throws IOException {
        JSONObject json;

        try {
            json = p.writer.toJSON(n);
        } catch (JSONException e) {
            throw new IOException(e);
        }

        p.map.put(Params.VIEW, toJettison(json));
    }

    public static float findMinAuthorizedSharability(final Principal user,
                                                     final float minSharability) {
        float minAuth = 0f;

        return Math.max(minSharability, minAuth);
    }

    protected boolean canWrite(final Principal user) {
        return true;
    }

    protected void addToHistory(final String rootId) {
        noteHistory.visit(rootId);
    }

    protected List<String> getHistory(final AtomGraph graph,
                                      final Filter filter) {
        return noteHistory.getHistory(100, true, graph, filter);
    }

    public static class RequestParams {
        public KeyIndexableGraph baseGraph;
        public Brain brain;
        public String data;
        public Integer height;
        public String file;
        public Filter filter;
        public String format;
        public boolean includeTypes;
        public JSONObject jsonView;
        public Map<String, Object> map;
        public Integer maxResults;
        public NoteParser parser;
        public String propertyName;
        public Object propertyValue;
        public NoteQueries queries;
        public String query;
        public NoteQueries.QueryType queryType;
        public Atom root;
        public String rootId;
        public NoteQueries.ViewStyle style;
        public String styleName;
        public Principal user;
        public Integer valueCutoff;
        public String wikiView;
        public NoteWriter writer;
    }
}
