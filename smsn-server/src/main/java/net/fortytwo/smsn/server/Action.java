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
import java.util.Map;
import java.util.logging.Logger;

public abstract class Action {
    protected static final Logger logger = Logger.getLogger(Action.class.getName());

    public abstract String getName();

    private static final int MAX_VIEW_HEIGHT = 7;

    private static final String CREATE_NEW_ATOM = "create-new-atom";

    protected abstract void performTransaction(RequestParams p) throws BadRequestException, RequestProcessingException;

    protected abstract boolean doesRead();

    protected abstract boolean doesWrite();

    private static final Map<Graph, Brain> brains = new HashMap<>();
    private static final Map<Graph, GraphWrapper> wrappers = new HashMap<>();

    private static final NoteHistory noteHistory = new NoteHistory();

    private synchronized static Brain getBrain(final GraphWrapper wrapper)
            throws Brain.BrainException {

        Brain brain = brains.get(wrapper.getGraph());

        if (null == brain) {
            logger.info("instantiating Extend-o-Brain with base graph " + wrapper.getGraph());
            AtomGraph bg = new PGAtomGraph(wrapper);
            brain = new Brain(bg);
            brain.startBackgroundTasks();
            brains.put(wrapper.getGraph(), brain);
        }

        return brain;
    }

    private synchronized static GraphWrapper getWrapper(final Neo4jGraph graph) {
        GraphWrapper wrapper = wrappers.get(graph);

        if (null == wrapper) {
            wrapper = new Neo4jGraphWrapper(graph);
            wrappers.put(graph, wrapper);
        }

        return wrapper;
    }

    public abstract void parseRequest(final JSONObject request, final RequestParams p) throws JSONException, BadRequestException;

    public void parseRequest(final String request, final RequestParams p) throws JSONException, BadRequestException {
        parseRequest(new JSONObject(request), p);
    }

    public void handleRequest(final RequestParams params) {

        setNonTransactionalParams(params);

        long before = System.currentTimeMillis();
        wrapTransactionAndExceptions(params);
        long after = System.currentTimeMillis();

        SemanticSynchrony.logInfo("completed " + getName() + " action in " + (after - before) + " ms");

        logActivity(params);
    }

    protected void addView(final Note n,
                           final RequestParams p) throws IOException {
        JSONObject json;

        try {
            json = p.getWriter().toJSON(n);
        } catch (JSONException e) {
            throw new IOException(e);
        }

        p.getMap().put(Params.VIEW, json);
    }

    public static float findMinAuthorizedSharability(final Principal user,
                                                     final float minSharability) {
        float minAuth = 0f;

        return Math.max(minSharability, minAuth);
    }

    public static RequestParams createParams(final Neo4jGraph graph) {
        RequestParams params = new RequestParams();

        setGraphWrapper(params, graph);
        params.setUser(() -> "none");

        return params;
    }

    private static void setGraphWrapper(final RequestParams params, final Neo4jGraph graph) {
        GraphWrapper wrapper = getWrapper(graph);
        params.setGraphWrapper(wrapper);
    }

    private boolean canWrite(final Principal user) {
        return true;
    }

    protected void addToHistory(final String rootId) {
        noteHistory.visit(rootId);
    }

    protected Iterable<Atom> getHistory(final AtomGraph graph,
                                        final Filter filter) {
        return noteHistory.getHistory(100, true, graph, filter);
    }

    private void wrapTransactionAndExceptions(final RequestParams params) {
        try {
            AtomGraph.wrapInTransaction(params.getBrain().getAtomGraph(), () -> {
                // must be done within the transaction, as it involves graph operations
                setTransactionalParams(params);

                performTransaction(params);
            });
        } catch (Exception e) {
            throw new RequestProcessingException(e);
        }
    }

    private void logActivity(final RequestParams params) {
        // Note: currently, all activities are logged, but the log is not immediately flushed
        //       unless the transaction succeeds.
        if (null != params.getBrain().getActivityLog()) {
            params.getBrain().getActivityLog().flush();
        }
    }

    private void checkAuthorized(final RequestParams p) throws AuthorizationException {
        if (doesWrite() && !canWrite(p.getUser())) {
            throw new AuthorizationException("user does not have permission to for write operations");
        }

        if (doesRead() && null == p.getFilter()) {
            throw new AuthorizationException("service reads from graph, but weight and sharability filter is not set");
        }
    }

    private void setNonTransactionalParams(final RequestParams params) {

        checkAuthorized(params);

        params.setMap(new HashMap<>());

        setWikiView(params);
        setBrain(params);
        setIO(params);
        setHeight(params);
        setFilter(params);
        setStyle(params);
    }

    private void setTransactionalParams(final RequestParams params) {
        setRoot(params);
        setTitle(params);
    }

    private void setWikiView(final RequestParams params) {
        if (null != params.getWikiView()) {
            // Force the use of the UTF-8 charset, which is apparently not chosen by Jersey
            // even when it is specified by the client in the Content-Type header, e.g.
            //    Content-Type: application/x-www-form-urlencoded;charset=UTF-8
            try {
                params.setWikiView(new String(params.getWikiView().getBytes("UTF-8")));
            } catch (UnsupportedEncodingException e) {
                throw new RequestProcessingException(e);
            }
        }
    }

    private void setBrain(final RequestParams params) {
        try {
            params.setBrain(getBrain(params.getGraphWrapper()));
        } catch (Brain.BrainException e) {
            throw new RequestProcessingException(e);
        }
    }

    private void setIO(final RequestParams params) {
        params.setQueries(new NoteQueries(params.getBrain()));
        params.setParser(new NoteReader());
        params.setWriter(new NoteWriter());
    }

    private void setHeight(final RequestParams params) {
        if (null != params.getHeight()) {
            if (params.getHeight() < 0) {
                throw new BadRequestException("height must be at least 0");
            }

            if (params.getHeight() > MAX_VIEW_HEIGHT) {
                throw new BadRequestException("height may not be more than 5");
            }

            params.getMap().put(Params.HEIGHT, "" + params.getHeight());
        }
    }

    private void setFilter(final RequestParams params) {
        if (null != params.getFilter()) {
            params.getMap().put(Params.MIN_SHARABILITY, "" + params.getFilter().getMinSharability());
            params.getMap().put(Params.MAX_SHARABILITY, "" + params.getFilter().getMaxSharability());
            params.getMap().put(Params.DEFAULT_SHARABILITY, "" + params.getFilter().getDefaultSharability());
            params.getMap().put(Params.MIN_WEIGHT, "" + params.getFilter().getMinWeight());
            params.getMap().put(Params.MAX_WEIGHT, "" + params.getFilter().getMaxWeight());
            params.getMap().put(Params.DEFAULT_WEIGHT, "" + params.getFilter().getDefaultWeight());
        }
    }

    private Atom createNewRoot(final RequestParams params) {
        Atom root = params.getBrain().getAtomGraph().createAtom(params.getFilter(), null);
        root.setValue("life, the universe, and everything");
        params.getBrain().getAtomGraph().reindexAtom(root);
        return root;
    }

    private void setRoot(final RequestParams params) {
        String rootId = params.getRootId();

        if (null != rootId) {
            Atom root = rootId.equals(CREATE_NEW_ATOM)
                    ? createNewRoot(params)
                    : params.getBrain().getAtomGraph().getAtomById(rootId);

            params.setRoot(root);

            if (null == params.getRoot()) {
                throw new BadRequestException("root of view does not exist: " + rootId);
            }

            if (null != params.getFilter() && !params.getFilter().isVisible(params.getRoot())) {
                throw new BadRequestException("root of view is not visible: " + rootId);
            }

            params.getMap().put(Params.ROOT, root.getId());
        }
    }

    private void setTitle(final RequestParams params) {
        params.getMap().put(Params.TITLE, null == params.getRoot()
                || null == params.getRoot().getValue()
                || 0 == params.getRoot().getValue().length() ? "[no title]" : params.getRoot().getValue());
    }

    private void setStyle(final RequestParams params) {
        String styleName = params.getStyleName();

        if (null != styleName) {
            params.setStyle(NoteQueries.lookupStyle(styleName));
            params.getMap().put(Params.STYLE, params.getStyle().getName());
        }
    }
}
