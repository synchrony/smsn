package net.fortytwo.extendo.typeatron.ripple.lib;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Filter;
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.extendo.typeatron.TypeatronControl;
import net.fortytwo.extendo.typeatron.ripple.ExtendoBrainClient;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RippleList;

import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GiveAtomMapping extends AtomMapping {

    private static final Logger logger = Logger.getLogger(GiveAtomMapping.class.getName());
    private final TypeatronControl typeatron;

    public GiveAtomMapping(final ExtendoBrainClient client,
                           final Filter filter,
                           final TypeatronControl typeatron) {
        super(client, filter);
        this.typeatron = typeatron;
    }

    public String[] getIdentifiers() {
        return new String[]{
                ExtendoLibrary.NS_2014_12 + "give-atom"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("atom", "the reference atom", true)};
    }

    public String getComment() {
        return "prepares an atom for the 'give' half of a hand-off gesture";
    }

    public void apply(RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {

        Object first = stack.getFirst();
        //stack = stack.getRest();

        Note n = toNote(first, 0, true);

        if (null == n) {
            logger.warning("can't give non-atom: " + first);
        } else {
            String value = n.getValue();
            if (null != value) {
                OSCMessage m = new OSCMessage(Extendo.EXO_GESTURE_GIVE);
                m.addArgument(typeatron.getAgent().getAgentUri().stringValue());
                m.addArgument(value);
                typeatron.getAgent().sendOSCMessageToFacilitator(m);
            }

            // keep the stack unchanged
            solutions.put(stack);
        }
    }
}
