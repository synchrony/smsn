package net.fortytwo.extendo.typeatron.ripple;

import net.fortytwo.ripple.RippleException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Environment {
    void speak(String message) throws RippleException;
    boolean verbose();
}
