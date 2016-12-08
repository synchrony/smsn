package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.Request;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

public class PushEventRequest extends Request {
    public final JSONObject jsonView;

    public PushEventRequest(JSONObject jsonStr, Principal user) throws JSONException {
        super(jsonStr, user);

        jsonView = json.getJSONObject(Params.VIEW);
    }
}
