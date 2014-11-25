package net.fortytwo.extendo.typeatron.ripple.lib;

import net.fortytwo.extendo.typeatron.TypeatronControl;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.PrimitiveStackMapping;
import net.fortytwo.ripple.model.RippleList;
import org.openrdf.model.URI;
import org.openrdf.model.Value;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class LaserPointerMapping extends PrimitiveStackMapping {

    private static final Logger logger = Logger.getLogger(LaserPointerMapping.class.getName());

    private final TypeatronControl typeatron;

    public LaserPointerMapping(final TypeatronControl typeatron) {
        this.typeatron = typeatron;
    }

    public String[] getIdentifiers() {
        return new String[]{
                ExtendoLibrary.NS_2014_12 + "point"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("thingPointedTo", "the thing pointed to or referenced", true)};
    }

    public String getComment() {
        return "points to or indicates an item";
    }

    public void apply(final RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection mc) throws RippleException {
        logger.log(Level.INFO, "executing the laser pointer mapping");

        Value thingPointedTo = mc.toRDF(stack.getFirst());

        if (null != thingPointedTo && thingPointedTo instanceof URI) {
            try {
                typeatron.pointTo((URI) thingPointedTo);
            } catch (Throwable t) {
                throw new RippleException(t);
            }

            // keep the thing pointed to on the stack
            solutions.put(stack);
        }
    }
}
