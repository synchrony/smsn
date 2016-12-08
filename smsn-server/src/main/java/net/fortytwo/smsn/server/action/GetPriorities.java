package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.action.requests.PrioritiesRequest;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

/**
 * A service for deriving a prioritized list of items in the knowledge base
 */
public class GetPriorities extends Action {

    @Override
    public String getName() {
        return "priorities";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        PrioritiesRequest r = new PrioritiesRequest(request);

        p.setFilter(r.getFilter());
        p.setMaxResults(r.maxResults);
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {

        Note n = p.getQueries().priorityView(p.getFilter(), p.getMaxResults(), p.getBrain().getPriorities());
        try {
            addView(n, p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }

}
