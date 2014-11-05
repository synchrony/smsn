package net.fortytwo.extendo.typeatron.ripple;

import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.io.RipplePrintStream;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.RDFValue;
import net.fortytwo.ripple.model.RippleValue;
import net.fortytwo.ripple.model.StackMapping;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ControlValue implements RippleValue {
    private final StackMapping mapping;

    public ControlValue(final StackMapping mapping) {
        this.mapping = mapping;
    }

    public RDFValue toRDF(final ModelConnection mc) throws RippleException {
        return null;
    }

    public StackMapping getMapping() {
        return mapping;
    }

    public void printTo(final RipplePrintStream p) throws RippleException {
        p.print("[" + this.getClass().getName() + "]");
    }

    public Type getType() {
        return Type.OPERATOR;
    }

    @Override
    public String toString() {
        return "Control(" + mapping + ")";
    }
}
