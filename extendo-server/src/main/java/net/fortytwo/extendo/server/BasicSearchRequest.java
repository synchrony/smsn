package net.fortytwo.extendo.server;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.UnsupportedEncodingException;
import java.security.Principal;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class BasicSearchRequest extends BasicViewRequest {

    private String query;

    public BasicSearchRequest(JSONObject json, Principal user) throws JSONException {
        super(json, user);

        query = this.json.getString(QUERY);

        try {
            // TODO: this doesn't solve the problem (that you can't search on queries with extended characters)
            query = new String(query.getBytes(), "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException(e);
        }
    }

    public String getQuery() {
        return query;
    }
}
