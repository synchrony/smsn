package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.smsn.brain.Filter;
import net.fortytwo.smsn.brain.Note;
import net.fortytwo.smsn.typeatron.ripple.ExtendoBrainClient;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;

import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GetAtomValueMapping extends AtomMapping {

    private static final Logger logger = Logger.getLogger(GetAtomValueMapping.class.getName());

    public GetAtomValueMapping(final ExtendoBrainClient client,
                               final Filter filter) {
        super(client, filter);
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "get-atom-value"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("atom", "the reference atom", true)};
    }

    public String getComment() {
        return "gets the @value property of an atom";
    }

    public void apply(RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {

        Object first = stack.getFirst();
        stack = stack.getRest();

        Note n = toNote(first, 0, true);

        if (null == n) {
            logger.warning("can't get @value of non-atom: " + first);
        } else {
            String value = n.getValue();
            if (null == value) {
                logger.warning("atom note has no @value: " + n);
            } else {
                // put both the @value property and the (synced) atom back on the stack
                solutions.accept(stack.push(n).push(value));
            }
        }
    }
}
