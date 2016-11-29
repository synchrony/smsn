package net.fortytwo.smsn.server;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.util.TypedProperties;
import org.apache.tinkerpop.gremlin.jsr223.Customizer;
import org.apache.tinkerpop.gremlin.jsr223.GremlinScriptEngine;
import org.apache.tinkerpop.gremlin.jsr223.GremlinScriptEngineFactory;
import org.apache.tinkerpop.gremlin.jsr223.GremlinScriptEngineManager;

import javax.script.ScriptEngine;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public class SmSnScriptEngineFactory implements GremlinScriptEngineFactory {
    private static final Logger logger = Logger.getLogger(SmSnScriptEngineFactory.class.getName());

    private static final String ENGINE_NAME = "smsn";
    private static final String LANGUAGE_NAME = "smsn";
    private static final String JSON = "application/json";
    private static final List<String> EXTENSIONS = Collections.singletonList("json");

    @Override
    public String getEngineName() {
        return ENGINE_NAME;
    }

    @Override
    public String getEngineVersion() {
        return getVersion();
    }

    @Override
    public List<String> getExtensions() {
        return EXTENSIONS;
    }

    @Override
    public List<String> getMimeTypes() {
        return Collections.singletonList(JSON);
    }

    @Override
    public List<String> getNames() {
        return Collections.singletonList(LANGUAGE_NAME);
    }

    @Override
    public String getLanguageName() {
        return LANGUAGE_NAME;
    }

    @Override
    public String getLanguageVersion() {
        return getVersion();
    }

    @Override
    public Object getParameter(String key) {
        switch (key) {
            case ScriptEngine.ENGINE:
                return this.getEngineName();
            case ScriptEngine.ENGINE_VERSION:
                return this.getEngineVersion();
            case ScriptEngine.NAME:
                return ENGINE_NAME;
            case ScriptEngine.LANGUAGE:
                return this.getLanguageName();
            case ScriptEngine.LANGUAGE_VERSION:
                return this.getLanguageVersion();
            default:
                return null;
        }
    }

    @Override
    public String getMethodCallSyntax(String obj, String m, String... args) {
        return null;
    }

    @Override
    public String getOutputStatement(String toDisplay) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getProgram(String... statements) {
        throw new UnsupportedOperationException();
    }

    @Override
    public GremlinScriptEngine getScriptEngine() {
        return new SmSnScriptEngine(this);
    }

    @Override
    public void setCustomizerManager(GremlinScriptEngineManager manager) {
        // TODO

        final List<Customizer> customizers =  manager.getCustomizers(ENGINE_NAME);

    }

    private String getVersion() {
        try {
            return SemanticSynchrony.getConfiguration().getString(SemanticSynchrony.VERSION);
        } catch (TypedProperties.PropertyException e) {
            logger.log(Level.WARNING, "couldn't get version", e);
            return "?";
        }
    }
}
