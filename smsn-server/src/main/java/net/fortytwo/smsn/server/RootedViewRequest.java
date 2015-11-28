package net.fortytwo.smsn.server;

import net.fortytwo.smsn.brain.Params;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class RootedViewRequest extends BasicViewRequest {

    private final String rootId;

    public RootedViewRequest(final JSONObject json,
                             final Principal user) throws JSONException {
        super(json, user);

        // note: root may be null
        rootId = optString(Params.ROOT);
    }

    public String getRootId() {
        return rootId;
    }
}
