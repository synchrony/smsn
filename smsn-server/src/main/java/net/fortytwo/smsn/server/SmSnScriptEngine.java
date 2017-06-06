package net.fortytwo.smsn.server;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.server.actions.NoAction;
import org.apache.commons.io.IOUtils;
import org.apache.tinkerpop.gremlin.jsr223.GremlinScriptEngine;
import org.apache.tinkerpop.gremlin.jsr223.GremlinScriptEngineFactory;
import org.apache.tinkerpop.gremlin.process.traversal.Bytecode;
import org.apache.tinkerpop.gremlin.process.traversal.Traversal;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.shaded.jackson.databind.ObjectMapper;
import org.json.JSONObject;

import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptException;
import java.io.*;
import java.util.Map;
import java.util.logging.Logger;

public class SmSnScriptEngine extends AbstractScriptEngine implements GremlinScriptEngine {

    protected static final Logger logger = Logger.getLogger(SmSnScriptEngine.class.getName());

    private static final String WARMUP_SCRIPT = "1+1";

    private final GremlinScriptEngineFactory factory;

    private final ObjectMapper objectMapper = createObjectMapper();

    public SmSnScriptEngine(GremlinScriptEngineFactory factory) {
        this.factory = factory;
    }

    @Override
    public Object eval(String script, ScriptContext context) throws ScriptException {

        Graph graph = getGraph(context);

        try {
            return handleRequest(script, graph);
        } catch (IOException e) {
            throw new ScriptException(e);
        }
    }

    @Override
    public Object eval(Reader reader, ScriptContext context) throws ScriptException {
        Graph graph = getGraph(context);

        try {
            String expression = readerToString(reader);
            return handleRequest(expression, graph);
        } catch (IOException e) {
            throw new ScriptException(e);
        }
    }

    private Action readAsJson(final String expression) throws IOException {
        return objectMapper.readValue(expression, Action.class);
    }

    private Action readAsYaml(final String expression) throws IOException {
        try (InputStream input = new StringBufferInputStream(expression)) {
            SemanticSynchrony.readConfigurationYaml(input);
        }

        logger.info("added configuration");
        return new NoAction();
    }

    private Action readAsWarmupScript(final String expression) {
        // this is ServerGremlinExecutor's warmup script hack; ignore
        return new NoAction();
    }

    private boolean isWarmupScript(final String expression) {
        return WARMUP_SCRIPT.equals(expression);
    }

    private boolean isJson(final String expression) {
        return expression.startsWith("{");
    }

    private String readerToString(final Reader reader) throws IOException {
        return IOUtils.toString(reader);
    }

    @Override
    public Traversal.Admin eval(Bytecode bytecode, Bindings bindings) throws ScriptException {
        throw new UnsupportedOperationException();
    }

    @Override
    public void put(String key, Object value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Bindings createBindings() {
        throw new UnsupportedOperationException();
    }

    @Override
    public GremlinScriptEngineFactory getFactory() {
        return factory;
    }

    private Graph getGraph(final ScriptContext context) {
        Graph graph = (Graph) context.getAttribute("graph");
        if (null == graph) {
            throw new IllegalStateException("expected graph not found");
        }

        return graph;
    }

    private JSONObject handleRequest(String actionStr, final Graph graph) throws IOException {
        Action action = deserializeRequest(actionStr);

        ActionContext params = new ActionPerformer(graph).perform(action);

        return toJson(params.getMap());
    }

    private Action deserializeRequest(final String requestStr) throws IOException {
        String expression = requestStr.trim();

        if (isWarmupScript(expression)) {
            return readAsWarmupScript(expression);
        } else if (isJson(expression)) {
            return readAsJson(expression);
        } else {
            // note: this is a hack.  This is currently how SmSn config YAML is loaded via Gremlin Server.
            return readAsYaml(expression);
        }
    }

    private JSONObject toJson(final Map<String, Object> map) {
        return new JSONObject(map);
    }

    public static ObjectMapper createObjectMapper() {
        // add any configuration here
        return new ObjectMapper();
    }

    public static class ActionPerformer {
        private final Graph graph;

        public ActionPerformer(final Graph graph) {
            this.graph = graph;
        }

        public ActionContext perform(final Action action) {
            ActionContext context = Action.createContext(graph);

            action.handleRequest(context);

            return context;
        }
    }
}
