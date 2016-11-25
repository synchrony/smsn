package net.fortytwo.smsn.server.requests;

import net.fortytwo.smsn.brain.Params;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

public class RootedViewRequest extends BasicViewRequest {

    private String rootId;

    public RootedViewRequest(final JSONObject json,
                             final Principal user) throws JSONException {
        super(json, user);

        // note: root may be null
        rootId = optString(Params.ROOT);
        // work around a Brain-mode quirk
        if (rootId.equals("null")) rootId = null;
    }

    public String getRootId() {
        return rootId;
    }
}
