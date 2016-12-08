package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.action.requests.FilteredResultsRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

public class PrioritiesRequest extends FilteredResultsRequest {
    private static final int DEFAULT_MAX_RESULTS = 100;

    public final int maxResults;

    public PrioritiesRequest(JSONObject json, Principal user) throws JSONException {
        super(json, user);

        maxResults = this.json.optInt(Params.MAX_RESULTS, DEFAULT_MAX_RESULTS);

        if (maxResults <= 0) {
            throw new JSONException(Params.MAX_RESULTS + " parameter must be a positive integer");
        }
    }
}
