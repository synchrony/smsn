package net.fortytwo.extendo.brainstem;

import org.openrdf.query.BindingSet;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface ComplexEventHandler {
    void handle(BindingSet result);
}
