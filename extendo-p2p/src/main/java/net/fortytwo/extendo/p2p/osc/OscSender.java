package net.fortytwo.extendo.p2p.osc;

import com.illposed.osc.OSCBundle;

/**
 * An abstraction for sending bundles of OSC messages to remote components
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface OscSender {
    /**
     * Sends an OSC bundle to a remote component
     *
     * @param bundle a bundle containing one or more OSC messages
     */
    void send(OSCBundle bundle);

    void close();
}
