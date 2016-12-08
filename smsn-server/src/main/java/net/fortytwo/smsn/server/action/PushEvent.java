package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.action.requests.PushEventRequest;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * A service for receiving and internalizing events
 */
public class PushEvent extends Action {

    @Override
    public String getName() {
        return "push-event";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        PushEventRequest r = new PushEventRequest(request);

        p.setJsonView(r.jsonView);
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        Note event = null;
        try {
            event = p.getParser().fromJSON(p.getJsonView());
        } catch (JSONException e) {
            throw new RequestProcessingException(e);
        }

        p.getBrain().getEventStack().push(event);
    }

    protected boolean doesRead() {
        return false;
    }

    protected boolean doesWrite() {
        // pushing of events is currently not considered writing... to the graph
        return false;
    }

}
