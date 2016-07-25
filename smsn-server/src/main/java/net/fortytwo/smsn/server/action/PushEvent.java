package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Request;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

/**
 * A service for receiving and internalizing events
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PushEvent extends Action {

    @Override
    public String getName() {
        return "push-event";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        PushEventRequest r = new PushEventRequest(request, p.user);

        p.jsonView = r.jsonView;
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        Note event = null;
        try {
            event = p.parser.fromJSON(p.jsonView);
        } catch (JSONException e) {
            throw new RequestProcessingException(e);
        }

        p.brain.getEventStack().push(event);
    }

    protected boolean doesRead() {
        return false;
    }

    protected boolean doesWrite() {
        // pushing of events is currently not considered writing... to the graph
        return false;
    }

    protected class PushEventRequest extends Request {
        public final JSONObject jsonView;

        public PushEventRequest(JSONObject jsonStr, Principal user) throws JSONException {
            super(jsonStr, user);

            jsonView = json.getJSONObject(Params.VIEW);
        }
    }
}
