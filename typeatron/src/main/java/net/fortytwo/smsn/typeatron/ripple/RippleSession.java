package net.fortytwo.smsn.typeatron.ripple;

import net.fortytwo.smsn.p2p.SideEffects;
import net.fortytwo.smsn.p2p.SmSnAgent;
import net.fortytwo.flow.Collector;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.Model;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;
import net.fortytwo.ripple.model.impl.sesame.SesameList;
import net.fortytwo.ripple.model.impl.sesame.SesameModel;
import net.fortytwo.ripple.query.LazyStackEvaluator;
import net.fortytwo.ripple.query.QueryEngine;
import net.fortytwo.ripple.query.StackEvaluator;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailException;
import org.openrdf.sail.memory.MemoryStore;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RippleSession {
    private static final int UNDO_REDO_DEPTH = 20;
    private final Sail sail;
    private final Model model;
    private final StackEvaluator evaluator;
    private final QueryEngine queryEngine;
    private final ModelConnection connection;

    private final UndoRedoStack<Collector<RippleList>> undoRedoStack;

    private final SmSnAgent agent;
    private final SideEffects environment;

    public RippleSession(final SmSnAgent agent,
                         final SideEffects environment) throws RippleException {
        this.agent = agent;
        this.environment = environment;
        this.undoRedoStack = new UndoRedoStack<Collector<RippleList>>(UNDO_REDO_DEPTH);
        undoRedoStack.done(new Collector<RippleList>());

        sail = new MemoryStore();
        try {
            sail.initialize();
        } catch (SailException e) {
            throw new RippleException(e);
        }
        model = new SesameModel(sail);
        evaluator = new LazyStackEvaluator();
        queryEngine = new QueryEngine(model, evaluator, System.out, System.err);
        connection = queryEngine.getConnection();
    }

    public UndoRedoStack<Collector<RippleList>> getUndoRedoStack() {
        return undoRedoStack;
    }

    public void close() throws RippleException {
        connection.finish();
        connection.close();
        try {
            sail.shutDown();
        } catch (SailException e) {
            throw new RippleException(e);
        }
    }

    public void push(final Object... nextValues) throws RippleException {
        System.out.println("pushing new values:");
        for (Object v : nextValues) {
            System.out.println("\t" + v);
        }

        Collector<RippleList> prevCollector = undoRedoStack.currentState();
        if (0 == prevCollector.size()) {
            // note: this modifies evaluation history, harmlessly
            prevCollector.put(SesameList.nilList());
        }
        Collector<RippleList> nextCollector = new Collector<RippleList>();

        System.out.println("\tbefore evaluation:");
        for (RippleList l : prevCollector) {
            RippleList cur = l;
            for (Object v : nextValues) {
                //System.out.println("pushing: " + v);
                cur = cur.push(v);
            }

            System.out.println("\t\t" + cur);

            evaluator.apply(cur, nextCollector, connection);
        }

        System.out.println("\tafter evaluation:");
        for (RippleList l : nextCollector) {
            System.out.println("\t\t" + l);
        }

        undoRedoStack.done(nextCollector);
    }

    public ModelConnection getModelConnection() {
        return connection;
    }
}
