package net.fortytwo.smsn.p2p.osc;

import com.illposed.osc.OSCMessage;

public interface OscMessageHandler {
    void handle(OSCMessage message);
}
