package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.pg.GraphWrapper;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.SmSnScriptEngine;
import net.fortytwo.smsn.server.SmSnScriptEngineFactory;

import java.io.IOException;

public abstract class ActionTestBase extends BrainTestBase {
    private SmSnScriptEngineFactory scriptEngineFactory = new SmSnScriptEngineFactory();
    private SmSnScriptEngine scriptEngine = new SmSnScriptEngine(scriptEngineFactory);

    @Override
    protected AtomGraph createAtomGraph() throws IOException {
        return createNeo4jAtomGraph();
    }

    protected void perform(final Action action) throws IOException {
        SmSnScriptEngine.ActionPerformer performer = new SmSnScriptEngine.ActionPerformer(graph);
        performer.perform(action);
    }
}
