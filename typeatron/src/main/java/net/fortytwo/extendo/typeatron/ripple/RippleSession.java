package net.fortytwo.extendo.typeatron.ripple;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.p2p.SideEffects;
import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.flow.Collector;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.Model;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;
import net.fortytwo.ripple.model.RippleValue;
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
    private final Sail sail;
    private final Model model;
    private final StackEvaluator evaluator;
    private final QueryEngine queryEngine;
    private final ModelConnection connection;

    private Collector<RippleList> prevCollector;
    private Collector<RippleList> curCollector;

    private final ExtendoAgent agent;
    private final SideEffects environment;

    public RippleSession(final ExtendoAgent agent,
                         final SideEffects environment) throws RippleException {
        this.agent = agent;
        this.environment = environment;

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

        prevCollector = new Collector<RippleList>();
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

    public void push(final RippleValue... nextValues) throws RippleException {
        System.out.println("pushing new values:");
        for (RippleValue v : nextValues) {
            System.out.println("\t" + v);
        }

        if (0 == prevCollector.size()) {
            prevCollector.put(SesameList.nilList());
        }
        curCollector = new Collector<RippleList>();

        System.out.println("\tbefore evaluation:");
        for (RippleList l : prevCollector) {
            RippleList cur = l;
            for (RippleValue v : nextValues) {
                //System.out.println("pushing: " + v);
                cur = cur.push(v);
            }

            System.out.println("\t\t" + cur);

            evaluator.apply(cur, curCollector, connection);
        }

        System.out.println("\tafter evaluation:");
        for (RippleList l : curCollector) {
            System.out.println("\t\t" + l);

            if (environment.verbose()) {
                if (agent.getFacilitatorConnection().isActive()) {
                    OSCMessage m = new OSCMessage("/exo/fctr/tt/stack");
                    m.addArgument(l.toString());
                    agent.sendOSCMessageToFacilitator(m);
                }
            }
        }
        prevCollector = curCollector;
    }

    public ModelConnection getModelConnection() {
        return connection;
    }
}
