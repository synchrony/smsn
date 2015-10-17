package net.fortytwo.smsn.p2p;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Pinger {
    public static final String
            PING = "ping",
            PING_REPLY = "ping-reply";

    private final Connection connection;

    private long before;

    private PingResultHandler resultHandler;

    // note: only one Pinger per connection; otherwise, conflicts will occur
    public Pinger(final Connection connection) {
        this.connection = connection;

        connection.registerHandler(PING_REPLY, new PingReplyHandler());
    }

    public synchronized void ping(final PingResultHandler handler) throws IOException, JSONException {
        resultHandler = handler;
        JSONObject message = new JSONObject();

        before = System.currentTimeMillis();

        connection.sendNow(PING, message);
    }

    private class PingReplyHandler implements MessageHandler {
        public void handle(final JSONObject message) {
            // stop the timer here, as soon as possible, before dealing with threads
            long after = System.currentTimeMillis();
            resultHandler.handleResult(after - before);
        }
    }

    public interface PingResultHandler {
        void handleResult(long delay);
    }
}
