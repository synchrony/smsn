package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.actions.requests.PushEventRequest;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

/**
 * A service for receiving and internalizing events
 */
public class PushEvent extends Action<PushEventRequest> {

    @Override
    public String getName() {
        return "push-event";
    }

    @Override
    public void parseRequest(final PushEventRequest request, final RequestParams p) throws IOException {
        try {
            p.setJsonView(new JSONObject(request.getView()));
        } catch (JSONException e) {
            throw new IOException(e);
        }
    }

    @Override
    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        Note event = null;
        try {
            event = p.getParser().fromJSON(p.getJsonView());
        } catch (JSONException e) {
            throw new RequestProcessingException(e);
        }

        p.getBrain().getEventStack().push(event);
    }

    @Override
    protected boolean doesRead() {
        return false;
    }

    @Override
    protected boolean doesWrite() {
        // pushing of events is currently not considered writing... to the graph
        return false;
    }

}
