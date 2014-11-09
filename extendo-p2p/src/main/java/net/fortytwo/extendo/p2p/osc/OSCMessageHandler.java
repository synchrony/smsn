package net.fortytwo.extendo.p2p.osc;

import com.illposed.osc.OSCMessage;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface OSCMessageHandler {
    void handle(OSCMessage message);
}
