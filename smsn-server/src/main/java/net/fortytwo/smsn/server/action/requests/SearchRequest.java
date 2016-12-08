package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.action.requests.BasicSearchRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

public class SearchRequest extends BasicSearchRequest {
    public final int valueCutoff;

    public SearchRequest(JSONObject json, Principal user) throws JSONException {
        super(json, user);

        valueCutoff = this.json.getInt(Params.VALUE_CUTOFF);
    }
}
