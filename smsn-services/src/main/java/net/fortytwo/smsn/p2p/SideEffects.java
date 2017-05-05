package net.fortytwo.smsn.p2p;

/**
 * A gateway to environment-dependent configuration and implementations of basic output functions
 */
public interface SideEffects {
    void speak(String message);
    void setStatus(String message);
}
