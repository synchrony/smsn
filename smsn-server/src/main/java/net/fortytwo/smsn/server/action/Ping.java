package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.Request;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.security.Principal;
import java.util.List;

/**
 * A minimal "ping" service
 */
public class Ping extends Action {

    @Override
    public String getName() {
        return "ping";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        p.map.put("time", String.valueOf(System.currentTimeMillis()));
    }

    protected boolean doesRead() {
        // getting events is currently not considered reading... from the graph
        return false;
    }

    protected boolean doesWrite() {
        return false;
    }

    protected class GetEventsRequest extends Request {
        public final int height;

        public GetEventsRequest(JSONObject json, Principal user) throws JSONException {
            super(json, user);

            height = this.json.getInt(Params.HEIGHT);
        }
    }
}
