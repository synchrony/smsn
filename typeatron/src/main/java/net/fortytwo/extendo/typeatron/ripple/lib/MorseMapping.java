package net.fortytwo.extendo.typeatron.ripple.lib;

import net.fortytwo.extendo.typeatron.TypeatronControl;
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
public class MorseMapping extends PrimitiveStackMapping {

    private static final Logger logger = Logger.getLogger(MorseMapping.class.getName());

    private final TypeatronControl typeatron;

    public MorseMapping(final TypeatronControl typeatron) {
        this.typeatron = typeatron;
    }

    public String[] getIdentifiers() {
        return new String[]{
                ExtendoLibrary.NS_2014_12 + "play-morse"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("text", "the text to encode", true)};
    }

    public String getComment() {
        return "plays an alphanumeric sequence in Morse code";
    }

    public void apply(final RippleList arg,
                      final Sink<RippleList> solutions,
                      final ModelConnection context) throws RippleException {
        logger.log(Level.INFO, "executing the Morse mapping");

        String message = arg.getFirst().toString();

        try {
            typeatron.sendMorseMessage(message);
        } catch (Throwable t) {
            throw new RippleException(t);
        }

        solutions.put(arg.getRest());
    }
}
