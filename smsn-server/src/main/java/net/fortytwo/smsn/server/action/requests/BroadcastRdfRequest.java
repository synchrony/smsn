package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.Request;
import org.json.JSONException;
import org.json.JSONObject;

public class BroadcastRdfRequest extends Request {
    public final String dataset;

    public BroadcastRdfRequest(JSONObject json) throws JSONException {
        super(json);

        dataset = this.json.getString(Params.DATASET);
    }
}
