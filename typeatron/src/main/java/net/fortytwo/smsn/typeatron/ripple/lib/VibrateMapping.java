package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.smsn.typeatron.TypeatronControl;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.PrimitiveStackMapping;
import net.fortytwo.ripple.model.RippleList;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class VibrateMapping extends PrimitiveStackMapping {
    private final TypeatronControl typeatron;

    public VibrateMapping(final TypeatronControl typeatron) {
        this.typeatron = typeatron;
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "vibrate"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[0];
    }

    public String getComment() {
        return "causes the vibration motor to emit a brief signal";
    }

    public void apply(RippleList arg, Sink<RippleList> solutions, ModelConnection context) throws RippleException {
        try {
            typeatron.sendVibroMessage(TypeatronControl.VIBRATE_MANUAL_MS);
        } catch (Throwable t) {
            throw new RippleException(t);
        }

        solutions.put(arg);
    }
}
