package net.fortytwo.extendo.p2p;

/**
 * A gateway to environment-dependent configuration and implementations of basic output functions
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface SideEffects {
    void speak(String message);
    void setStatus(String message);
}
