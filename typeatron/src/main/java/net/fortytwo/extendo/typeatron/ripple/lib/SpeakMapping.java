package net.fortytwo.extendo.typeatron.ripple.lib;

import net.fortytwo.extendo.p2p.SideEffects;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.PrimitiveStackMapping;
import net.fortytwo.ripple.model.RippleList;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class SpeakMapping extends PrimitiveStackMapping {

    private static final Logger logger = Logger.getLogger(SpeakMapping.class.getName());

    private final SideEffects environment;

    public SpeakMapping(final SideEffects context) {
        this.environment = context;
    }

    public String[] getIdentifiers() {
        return new String[]{
                BrainstemLibrary.NS_2014_04 + "speak"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("text", "the text to be spoken", true)};
    }

    public String getComment() {
        return "speaks a line of text";
    }

    public void apply(final RippleList arg,
                      final Sink<RippleList> solutions,
                      final ModelConnection context) throws RippleException {
        logger.log(Level.INFO, "executing the speak mapping");

        String text = arg.getFirst().toString();

        try {
            environment.speak(text);
        } catch (Throwable t) {
            throw new RippleException(t);
        }

        solutions.put(arg.getRest());
    }
}
