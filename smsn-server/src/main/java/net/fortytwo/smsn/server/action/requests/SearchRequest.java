package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.brain.Params;
import org.json.JSONException;
import org.json.JSONObject;

public class SearchRequest extends BasicSearchRequest {
    public final int valueCutoff;

    public SearchRequest(JSONObject json) throws JSONException {
        super(json);

        valueCutoff = this.json.getInt(Params.VALUE_CUTOFF);
    }
}
