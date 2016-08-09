package net.fortytwo.smsn.p2p.osc;

import com.illposed.osc.OSCBundle;
import com.illposed.osc.OSCPortOut;

import java.io.IOException;
import java.net.InetAddress;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class UdpOscSender implements OscSender {
    private static final Logger logger = Logger.getLogger(UdpOscSender.class.getName());

    private final OSCPortOut oscPortOut;

    public UdpOscSender(final String hostOut,
                        final int portOut) throws UnknownHostException, SocketException {
        InetAddress outAddress = null == hostOut || hostOut.equals("localhost") || hostOut.equals("127.0.0.1")
                ? InetAddress.getLocalHost() : InetAddress.getByName(hostOut);
        oscPortOut = new OSCPortOut(outAddress, portOut);
    }

    @Override
    public void send(OSCBundle bundle) {
        try {
            oscPortOut.send(bundle);
        } catch (IOException e) {
            logger.log(Level.WARNING, "failed to send OSC bundle", e);
        }
    }

    @Override
    public void close() {
        // no-op
    }
}

