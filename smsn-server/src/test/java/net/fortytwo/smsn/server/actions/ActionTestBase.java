package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.SmSnScriptEngine;
import net.fortytwo.smsn.server.SmSnScriptEngineFactory;
import org.json.JSONObject;

import java.io.IOException;

public abstract class ActionTestBase extends BrainTestBase {
    private final SmSnScriptEngineFactory scriptEngineFactory = new SmSnScriptEngineFactory();
    private SmSnScriptEngine scriptEngine = new SmSnScriptEngine(scriptEngineFactory);

    @Override
    protected TopicGraph createAtomGraph() throws IOException {
        //return createNeo4jAtomGraph();
        return createTinkerAtomGraph();
    }

    protected ActionContext perform(final Action action) throws IOException {
        SmSnScriptEngine.ActionPerformer performer = new SmSnScriptEngine.ActionPerformer(graph);
        return performer.perform(action);
    }

    protected JSONObject getView(final ActionContext context) {
        return (JSONObject) context.getMap().get(Params.VIEW);
    }

    protected Atom createAtomWithTitle(final String title) {
        Atom atom = topicGraph.createAtomWithProperties(Filter.noFilter(), null);
        atom.setTitle(title);
        return atom;
    }
}
