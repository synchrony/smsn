package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;

import java.util.logging.Logger;

public class SetAtomWeightMapping extends AtomMapping {

    private static final Logger logger = Logger.getLogger(SetAtomWeightMapping.class.getName());

    public SetAtomWeightMapping(final BrainClient client,
                                final Filter filter) {
        super(client, filter);
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "set-atom-weight"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{
                new Parameter("atom", "the reference atom", true),
                new Parameter("weight", "the new weight", true)};
    }

    public String getComment() {
        return "sets the @weight property of an atom";
    }

    public void apply(RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {

        Object value = stack.getFirst();
        stack = stack.getRest();
        Object no = stack.getFirst();
        stack = stack.getRest();

        Note n = toNote(no, 0, false);

        if (null == n) {
            logger.warning("can't set @weight of non-atom: " + no);
        } else {
            Float f = sharabilityOrWeightFromArgument(value, mc);

            setProperty(n, SemanticSynchrony.WEIGHT, "" + f);

            // put the atom back on the stack
            solutions.accept(stack.push(n));
        }
    }
}
