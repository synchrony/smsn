package net.fortytwo.extendo.typeatron.ripple.lib;

import net.fortytwo.extendo.typeatron.TypeatronControl;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.PrimitiveStackMapping;
import net.fortytwo.ripple.model.RippleList;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GetLightLevelMapping extends PrimitiveStackMapping {
    private final TypeatronControl typeatron;

    public GetLightLevelMapping(final TypeatronControl typeatron) {
        this.typeatron = typeatron;
    }

    public String[] getIdentifiers() {
        return new String[]{
                ExtendoLibrary.NS_2014_12 + "get-light-level"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[0];
    }

    public String getComment() {
        return "fetches the current light level from the photoresistor";
    }

    public void apply(RippleList arg,
                      Sink<RippleList> solutions,
                      ModelConnection mc) throws RippleException {
        try {
            typeatron.sendPhotoresistorGetCommand();
        } catch (Throwable t) {
            throw new RippleException(t);
        }

        // For now, we decouple the response from the request, pushing the light level onto all stacks in the session.
        // Eventually, it would be best to push the light level only onto *this* stack (arg).

        solutions.put(arg);
    }
}
