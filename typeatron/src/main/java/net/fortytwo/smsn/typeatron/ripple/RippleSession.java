package net.fortytwo.smsn.typeatron.ripple;

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
import org.eclipse.rdf4j.sail.Sail;
import org.eclipse.rdf4j.sail.SailException;
import org.eclipse.rdf4j.sail.memory.MemoryStore;

public class RippleSession {
    private static final int UNDO_REDO_DEPTH = 20;
    private final Sail sail;
    private final StackEvaluator evaluator;
    private final ModelConnection connection;

    private final UndoRedoStack<Collector<RippleList>> undoRedoStack;

    public RippleSession() throws RippleException {
        this.undoRedoStack = new UndoRedoStack<>(UNDO_REDO_DEPTH);
        undoRedoStack.done(new Collector<>());

        sail = new MemoryStore();
        try {
            sail.initialize();
        } catch (SailException e) {
            throw new RippleException(e);
        }
        Model model = new SesameModel(sail);
        evaluator = new LazyStackEvaluator();
        QueryEngine queryEngine = new QueryEngine(model, evaluator, System.out, System.err);
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
            prevCollector.accept(SesameList.nilList());
        }
        Collector<RippleList> nextCollector = new Collector<>();

        System.out.println("\tbefore evaluation:");
        for (RippleList l : prevCollector) {
            RippleList cur = l;
            for (Object v : nextValues) {
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
