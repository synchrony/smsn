package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.Request;
import org.json.JSONException;
import org.json.JSONObject;

public class GetEventsRequest extends Request {
    public final int height;

    public GetEventsRequest(JSONObject json) throws JSONException {
        super(json);

        height = this.json.getInt(Params.HEIGHT);
    }
}
