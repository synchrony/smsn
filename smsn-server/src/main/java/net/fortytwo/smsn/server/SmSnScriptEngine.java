package net.fortytwo.smsn.server;

import net.fortytwo.smsn.SemanticSynchrony;
import org.apache.tinkerpop.gremlin.jsr223.GremlinScriptEngine;
import org.apache.tinkerpop.gremlin.jsr223.GremlinScriptEngineFactory;
import org.apache.tinkerpop.gremlin.neo4j.structure.Neo4jGraph;
import org.apache.tinkerpop.gremlin.process.traversal.Bytecode;
import org.apache.tinkerpop.gremlin.process.traversal.Traversal;
import org.apache.tinkerpop.shaded.jackson.databind.ObjectMapper;
import org.json.JSONObject;

import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptException;
import java.io.IOException;
import java.io.Reader;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

public class SmSnScriptEngine extends AbstractScriptEngine implements GremlinScriptEngine {

    protected static final Logger logger = Logger.getLogger(SmSnScriptEngine.class.getName());

    private final GremlinScriptEngineFactory factory;

    private final ObjectMapper objectMapper = new ObjectMapper();

    public SmSnScriptEngine(GremlinScriptEngineFactory factory) {
        this.factory = factory;
    }

    @Override
    public Object eval(String script, ScriptContext context) throws ScriptException {

        Neo4jGraph graph = getNeo4jGraph(context);

        try {
            return handleRequest(script, graph);
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    // note: this is a hack.  This is currently how config properties are loaded.
    @Override
    public Object eval(Reader reader, ScriptContext context) throws ScriptException {
        Properties properties = new Properties();
        try {
            properties.load(reader);
        } catch (IOException e) {
            throw new ScriptException(e);
        }

        SemanticSynchrony.addConfiguration(properties);
        return "added " + properties.size() + " configurations properties";
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

    private Neo4jGraph getNeo4jGraph(final ScriptContext context) {
        Neo4jGraph graph = (Neo4jGraph) context.getAttribute("graph");
        if (null == graph) {
            throw new IllegalStateException("expected Neo4j graph not found");
        }

        return graph;
    }

    private JSONObject handleRequest(String actionStr, Neo4jGraph graph) throws IOException {
        Action action = deserializeRequest(actionStr);
        RequestParams params = Action.createParams(graph);

        action.parseRequest(params);

        action.handleRequest(params);

        return toJson(params.getMap());
    }

    private Action deserializeRequest(final String requestStr) throws IOException {
        return objectMapper.readValue(requestStr, Action.class);
    }

    private JSONObject toJson(final Map<String, Object> map) {
        return new JSONObject(map);
    }
}
