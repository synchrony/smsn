package net.fortytwo.smsn.server;

import org.json.JSONException;
import org.json.JSONObject;

public class Request {

    protected final JSONObject json;

    public Request(final JSONObject json) {
        this.json = json;
    }

    protected String optString(final String key) throws JSONException {
        return json.has(key) ? json.getString(key) : null;
    }
}
