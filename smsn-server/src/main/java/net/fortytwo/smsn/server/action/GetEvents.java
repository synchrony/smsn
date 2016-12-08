package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.action.requests.GetEventsRequest;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.util.List;

/**
 * A service for retrieving the stack of recently pushed events
 */
public class GetEvents extends Action {

    @Override
    public String getName() {
        return "get-events";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        GetEventsRequest r = new GetEventsRequest(request, p.getUser());

        p.setHeight(r.height);
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        List<Note> events = p.getBrain().getEventStack().getEvents();

        Note view = new Note();
        view.setValue("event stack");

        for (Note n : events) {
            Note e = new Note(n);
            e.truncate(p.getHeight());
            view.addChild(e);
        }

        try {
            addView(view, p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    protected boolean doesRead() {
        // getting events is currently not considered reading... from the graph
        return false;
    }

    protected boolean doesWrite() {
        return false;
    }
}
