package net.fortytwo.smsn.server;

import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

public class Request {

    protected final JSONObject json;
    protected final Principal user;

    public Request(final JSONObject json,
                   final Principal user) {
        this.json = json;
        this.user = user;
    }

    protected String optString(final String key) throws JSONException {
        return json.has(key) ? json.getString(key) : null;
    }
}
