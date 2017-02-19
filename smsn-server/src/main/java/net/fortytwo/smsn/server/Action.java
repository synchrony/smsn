package net.fortytwo.smsn.server;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.History;
import net.fortytwo.smsn.brain.TreeViews;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.io.json.JsonParser;
import net.fortytwo.smsn.brain.io.json.JsonPrinter;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.pg.GraphWrapper;
import net.fortytwo.smsn.brain.model.pg.Neo4jGraphWrapper;
import net.fortytwo.smsn.brain.model.pg.PGTopicGraph;
import net.fortytwo.smsn.brain.model.pg.TinkerGraphWrapper;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.apache.tinkerpop.gremlin.neo4j.structure.Neo4jGraph;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;
import org.apache.tinkerpop.shaded.jackson.annotation.JsonIgnoreProperties;
import org.apache.tinkerpop.shaded.jackson.annotation.JsonTypeInfo;
import org.json.JSONObject;

import javax.validation.constraints.NotNull;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY, property = "action")
@JsonIgnoreProperties(ignoreUnknown = true)
public abstract class Action {
    protected static final Logger logger = Logger.getLogger(Action.class.getName());

    private static final int MAX_VIEW_HEIGHT = 7;

    private static final String CREATE_NEW_ATOM = "create-new-atom";

    private static final Map<Graph, Brain> brains = new HashMap<>();
    private static final Map<Graph, GraphWrapper> wrappers = new HashMap<>();

    private static final History history = new History();

    @NotNull
    private String action;

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public abstract void parseRequest(final RequestParams params) throws IOException, BadRequestException;

    protected abstract void performTransaction(RequestParams params) throws BadRequestException, RequestProcessingException;

    protected abstract boolean doesRead();

    protected abstract boolean doesWrite();

    public synchronized static Brain getBrain(final GraphWrapper wrapper)
            throws Brain.BrainException {

        Brain brain = brains.get(wrapper.getGraph());

        if (null == brain) {
            logger.info("instantiating Extend-o-Brain with base graph " + wrapper.getGraph());
            TopicGraph bg = new PGTopicGraph(wrapper);
            brain = new Brain(bg);
            brain.startBackgroundTasks();
            brains.put(wrapper.getGraph(), brain);
        }

        return brain;
    }

    public synchronized static GraphWrapper getWrapper(final Graph graph) {
        GraphWrapper wrapper = wrappers.get(graph);

        if (null == wrapper) {
            wrapper = wrap(graph);
            wrappers.put(graph, wrapper);
        }

        return wrapper;
    }

    private static GraphWrapper wrap(final Graph graph) {
        return graph instanceof TinkerGraph
            ? new TinkerGraphWrapper((TinkerGraph) graph)
            : new Neo4jGraphWrapper((Neo4jGraph) graph);
    }

    public void handleRequest(final RequestParams params) {

        setNonTransactionalParams(params);

        long before = System.currentTimeMillis();
        wrapTransactionAndExceptions(params);
        long after = System.currentTimeMillis();

        SemanticSynchrony.logInfo("completed " + getClass().getSimpleName() + " action in " + (after - before) + " ms");

        logActivity(params);
    }

    protected void addView(final Note n,
                           final RequestParams params) throws IOException {
        JSONObject json;

        json = params.getJsonPrinter().toJson(n);

        params.getMap().put(Params.VIEW, json);
    }

    public static RequestParams createParams(final Graph graph) {
        RequestParams params = new RequestParams();

        setGraphWrapper(params, graph);

        return params;
    }

    private static void setGraphWrapper(final RequestParams params, final Graph graph) {
        GraphWrapper wrapper = getWrapper(graph);
        params.setGraphWrapper(wrapper);
    }

    protected void addToHistory(final String rootId) {
        history.visit(rootId);
    }

    protected Iterable<Atom> getHistory(final TopicGraph graph,
                                        final Filter filter) {
        return history.getHistory(100, graph, filter);
    }

    private void wrapTransactionAndExceptions(final RequestParams params) {
        try {
            TopicGraph.wrapInTransaction(params.getBrain().getTopicGraph(), () -> {
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

    private void setNonTransactionalParams(final RequestParams params) {

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
        if (null != params.getView()) {
            // Force the use of the UTF-8 charset, which is apparently not chosen by Jersey
            // even when it is specified by the client in the Content-Type header, e.g.
            //    Content-Type: application/x-www-form-urlencoded;charset=UTF-8
            try {
                params.setView(new String(params.getView().getBytes("UTF-8")));
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
        params.setQueries(new TreeViews(params.getBrain()));
        params.setWikiParser(new WikiParser());
        params.setJsonParser(new JsonParser());
        params.setJsonPrinter(new JsonPrinter());
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
        Atom root = params.getBrain().getTopicGraph().createAtomWithProperties(params.getFilter(), null);
        root.setTitle("life, the universe, and everything");
        params.getBrain().getTopicGraph().reindexAtom(root);
        return root;
    }

    private void setRoot(final RequestParams params) {
        String rootId = params.getRootId();

        if (null != rootId) {
            Atom root = rootId.equals(CREATE_NEW_ATOM)
                    ? createNewRoot(params)
                    : params.getBrain().getTopicGraph().getAtomById(rootId);

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
        params.getMap().put(Params.VIEW_TITLE, null == params.getRoot()
                || null == params.getRoot().getTitle()
                || 0 == params.getRoot().getTitle().length() ? "[no title]" : params.getRoot().getTitle());
    }

    private void setStyle(final RequestParams params) {
        String styleName = params.getStyleName();

        if (null != styleName) {
            params.setStyle(TreeViews.lookupStyle(styleName));
            params.getMap().put(Params.STYLE, params.getStyle().getName());
        }
    }
}
