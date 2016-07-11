package net.fortytwo.smsn.p2p;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PingAnswerer {
    private final Connection connection;

    public PingAnswerer(Connection connection) {
        this.connection = connection;

        connection.registerHandler(Pinger.PING, new PingHandler());
    }

    private class PingHandler implements MessageHandler {
        public void handle(final JSONObject message) throws MessageHandlerException {
            // empty body; nothing is necessary
            JSONObject body = new JSONObject();

            try {
                connection.sendNow(Pinger.PING_REPLY, body);
            } catch (JSONException | IOException e) {
                throw new MessageHandlerException(e);
            }
        }
    }
}
