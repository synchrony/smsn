package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.smsn.p2p.SideEffects;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.PrimitiveStackMapping;
import net.fortytwo.ripple.model.RippleList;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class SpeakMapping extends PrimitiveStackMapping {

    private final SideEffects environment;

    public SpeakMapping(final SideEffects context) {
        this.environment = context;
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "speak"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("text", "the text to be spoken", true)};
    }

    public String getComment() {
        return "speaks a line of text";
    }

    public void apply(final RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection context) throws RippleException {

        String text = context.toString(stack.getFirst());

        try {
            environment.speak(text);
        } catch (Throwable t) {
            throw new RippleException(t);
        }

        solutions.put(stack);
    }
}
