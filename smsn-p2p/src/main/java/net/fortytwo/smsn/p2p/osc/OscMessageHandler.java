package net.fortytwo.smsn.p2p.osc;

import com.illposed.osc.OSCMessage;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface OscMessageHandler {
    void handle(OSCMessage message);
}
