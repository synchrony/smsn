package net.fortytwo.smsn.server;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.NoteHistory;
import net.fortytwo.smsn.brain.NoteQueries;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.pg.GraphWrapper;
import net.fortytwo.smsn.brain.model.pg.Neo4jGraphWrapper;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;
import net.fortytwo.smsn.brain.wiki.NoteReader;
import net.fortytwo.smsn.brain.wiki.NoteWriter;
import net.fortytwo.smsn.server.error.AuthorizationException;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import org.apache.tinkerpop.gremlin.neo4j.structure.Neo4jGraph;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.Principal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

public abstract class Action {
    protected static final Logger logger = Logger.getLogger(Action.class.getName());

    public abstract String getName();

    private static final int MAX_VIEW_HEIGHT = 7;

    protected abstract void performTransaction(RequestParams p) throws BadRequestException, RequestProcessingException;

    protected abstract boolean doesRead();

    protected abstract boolean doesWrite();

    private static final Map<Graph, Brain> brains = new HashMap<>();

    private static final NoteHistory noteHistory = new NoteHistory();

    private synchronized static Brain getBrain(final GraphWrapper wrapper)
            throws Brain.BrainException {

        Brain b = brains.get(wrapper.getGraph());

        if (null == b) {
            logger.info("instantiating Extend-o-Brain with base graph " + wrapper.getGraph());
            AtomGraph bg = new PGAtomGraph(wrapper);
            b = new Brain(bg);
            b.startBackgroundTasks();
            brains.put(wrapper.getGraph(), b);
        }

        return b;
    }

    public abstract void parseRequest(final JSONObject request, final RequestParams p) throws JSONException, BadRequestException;

    public void parseRequest(final String request, final RequestParams p) throws JSONException, BadRequestException {
        parseRequest(new JSONObject(request), p);
    }

    private void checkAuthorized(final RequestParams p) throws AuthorizationException {
        if (doesWrite() && !canWrite(p.user)) {
            throw new AuthorizationException("user does not have permission to for write operations");
        }

        if (doesRead() && null == p.filter) {
            throw new AuthorizationException("service reads from graph, but weight and sharability filter is not set");
        }
    }

    public void handleRequestInternal(final RequestParams params)
            throws AuthorizationException, BadRequestException, RequestProcessingException {

        checkAuthorized(params);

        params.map = new HashMap<>();

        setWikiView(params);
        setBrain(params);
        setIO(params);
        setHeight(params);
        setFilter(params);
        setRoot(params);
        setTitle(params);
        setStyle(params);

        // Force manual transaction mode (provided that the graph is transactional)
        boolean manual = doesWrite() && params.graphWrapper.isTransactional();

        boolean normal = false;

        try {
            performTransaction(params);

            normal = true;

            // Note: currently, all activities are logged, but the log is not immediately flushed
            //       unless the transaction succeeds.
            if (null != params.brain.getActivityLog()) {
                params.brain.getActivityLog().flush();
            }
        } finally {
            if (doesWrite()) {
                if (manual) {
                    if (normal) {
                        params.graphWrapper.commit();
                    } else {
                        SemanticSynchrony.logWarning("rolling back transaction");
                        params.graphWrapper.rollback();
                    }
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

    public static RequestParams createParams(final Neo4jGraph graph) {
        RequestParams p = new RequestParams();
        p.graphWrapper = new Neo4jGraphWrapper(graph);
        p.user = () -> "none";

        return p;
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

    private void setWikiView(final RequestParams params) {
        if (null != params.wikiView) {
            // Force the use of the UTF-8 charset, which is apparently not chosen by Jersey
            // even when it is specified by the client in the Content-Type header, e.g.
            //    Content-Type: application/x-www-form-urlencoded;charset=UTF-8
            try {
                params.wikiView = new String(params.wikiView.getBytes("UTF-8"));
            } catch (UnsupportedEncodingException e) {
                throw new RequestProcessingException(e);
            }
        }
    }

    private void setBrain(final RequestParams params) {
        try {
            params.brain = getBrain(params.graphWrapper);
        } catch (Brain.BrainException e) {
            throw new RequestProcessingException(e);
        }
    }

    private void setIO(final RequestParams params) {
        params.queries = new NoteQueries(params.brain);
        params.parser = new NoteReader();
        params.writer = new NoteWriter();
    }

    private void setHeight(final RequestParams params) {
        if (null != params.height) {
            if (params.height < 0) {
                throw new BadRequestException("height must be at least 0");
            }

            if (params.height > MAX_VIEW_HEIGHT) {
                throw new BadRequestException("height may not be more than 5");
            }

            params.map.put(Params.HEIGHT, "" + params.height);
        }
    }

    private void setFilter(final RequestParams params) {
        if (null != params.filter) {
            params.map.put(Params.MIN_SHARABILITY, "" + params.filter.getMinSharability());
            params.map.put(Params.MAX_SHARABILITY, "" + params.filter.getMaxSharability());
            params.map.put(Params.DEFAULT_SHARABILITY, "" + params.filter.getDefaultSharability());
            params.map.put(Params.MIN_WEIGHT, "" + params.filter.getMinWeight());
            params.map.put(Params.MAX_WEIGHT, "" + params.filter.getMaxWeight());
            params.map.put(Params.DEFAULT_WEIGHT, "" + params.filter.getDefaultWeight());
        }
    }

    private void setRoot(final RequestParams params) {
        String rootId = params.rootId;

        if (null != rootId) {
            params.root = params.brain.getAtomGraph().getAtom(rootId);

            if (null == params.root) {
                throw new BadRequestException("root of view does not exist: " + rootId);
            }

            if (null != params.filter && !params.filter.isVisible(params.root)) {
                throw new BadRequestException("root of view is not visible: " + rootId);
            }

            params.map.put(Params.ROOT, rootId);
        }
    }

    private void setTitle(final RequestParams params) {
        params.map.put(Params.TITLE, null == params.root
                || null == params.root.getValue()
                || 0 == params.root.getValue().length() ? "[no title]" : params.root.getValue());
    }

    private void setStyle(final RequestParams params) {
        String styleName = params.styleName;

        if (null != styleName) {
            params.style = NoteQueries.lookupStyle(styleName);
            params.map.put(Params.STYLE, params.style.getName());
        }
    }

    public static class RequestParams {
        public GraphWrapper graphWrapper;
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
        public NoteReader parser;
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
