package net.fortytwo.smsn.server.requests;

import net.fortytwo.smsn.brain.NoteQueries;
import net.fortytwo.smsn.brain.Params;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.UnsupportedEncodingException;
import java.security.Principal;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class BasicSearchRequest extends BasicViewRequest {

    private String query;
    private final NoteQueries.QueryType queryType;

    public BasicSearchRequest(JSONObject json, Principal user) throws JSONException {
        super(json, user);

        queryType = NoteQueries.QueryType.valueOf(this.json.getString(Params.QUERY_TYPE));
        if (null == queryType) {
            // TODO: use a more appropriate exception
            throw new JSONException("no query type specified");
        }

        query = this.json.getString(Params.QUERY);

        try {
            // TODO: this doesn't solve the problem (that you can't search on queries with extended characters)
            query = new String(query.getBytes(), "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException(e);
        }
    }

    public NoteQueries.QueryType getQueryType() {
        return queryType;
    }

    public String getQuery() {
        return query;
    }
}
