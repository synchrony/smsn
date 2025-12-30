package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.typeatron.TypeatronControl;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;
import org.eclipse.rdf4j.model.IRI;

import java.util.logging.Level;
import java.util.logging.Logger;

public class LaserPointerMapping extends NoteMapping {

    private static final Logger logger = Logger.getLogger(LaserPointerMapping.class.getName());

    private final TypeatronControl typeatron;

    public LaserPointerMapping(final BrainClient client,
                               final Filter filter,
                               final TypeatronControl typeatron) {
        super(client, filter);
        this.typeatron = typeatron;
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "point"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("referent", "the thing pointed to or referenced", true)};
    }

    public String getComment() {
        return "points to or indicates an item";
    }

    public void apply(final RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {
        Object first = stack.getFirst();
        TreeNode<Link> n = toTree(first, 0, true);

        if (null == n) {
            logger.warning("can't point to non-note: " + first);

            // soft fail; propagate the stack even if we couldn't point
        } else {
            IRI iri = iriOf(n);

            // value is informational; it is used only for development/debugging purposes
            String value = TreeViews.getTitle(n);

            logger.log(Level.INFO, "pointing to " + iri + " (" + value + ")");

            try {
                typeatron.pointTo(iri);
            } catch (Throwable t) {
                throw new RippleException(t);
            }
        }

        // keep the stack unchanged
        solutions.accept(stack);
    }
}
