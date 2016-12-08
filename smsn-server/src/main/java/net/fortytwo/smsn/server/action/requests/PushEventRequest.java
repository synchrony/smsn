package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.Request;
import org.json.JSONException;
import org.json.JSONObject;

public class PushEventRequest extends Request {
    public final JSONObject jsonView;

    public PushEventRequest(JSONObject jsonStr) throws JSONException {
        super(jsonStr);

        jsonView = json.getJSONObject(Params.VIEW);
    }
}
