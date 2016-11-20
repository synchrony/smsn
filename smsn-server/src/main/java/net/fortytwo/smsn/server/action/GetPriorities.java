package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.server.requests.FilteredResultsRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.security.Principal;

/**
 * A service for deriving a prioritized list of items in the knowledge base
 */
public class GetPriorities extends Action {

    private static final int DEFAULT_MAX_RESULTS = 100;

    @Override
    public String getName() {
        return "priorities";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        PrioritiesRequest r = new PrioritiesRequest(request, p.getUser());

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

    protected class PrioritiesRequest extends FilteredResultsRequest {
        public final int maxResults;

        public PrioritiesRequest(JSONObject json, Principal user) throws JSONException {
            super(json, user);

            maxResults = this.json.optInt(Params.MAX_RESULTS, DEFAULT_MAX_RESULTS);

            if (maxResults <= 0) {
                throw new JSONException(Params.MAX_RESULTS + " parameter must be a positive integer");
            }
        }
    }
}
