package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Request;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.security.Principal;
import java.util.List;

/**
 * A service for retrieving the stack of recently pushed events
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GetEvents extends Action {

    @Override
    public String getName() {
        return "get-events";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        GetEventsRequest r = new GetEventsRequest(request, p.user);

        p.height = r.height;
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        List<Note> events = p.brain.getEventStack().getEvents();

        Note view = new Note();
        view.setValue("event stack");

        for (Note n : events) {
            Note e = new Note(n);
            e.truncate(p.height);
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

    protected class GetEventsRequest extends Request {
        public final int height;

        public GetEventsRequest(JSONObject json, Principal user) throws JSONException {
            super(json, user);

            height = this.json.getInt(Params.HEIGHT);
        }
    }
}
