package net.fortytwo.extendo.server;

import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class BasicViewRequest extends FilteredResultsRequest {
    private final int depth;
    private final String styleName;

    public BasicViewRequest(final JSONObject json,
                            final Principal user) throws JSONException {
        super(json, user);

        depth = this.json.getInt(DEPTH);
        styleName = this.json.getString(STYLE);
    }

    public int getDepth() {
        return depth;
    }

    public String getStyleName() {
        return styleName;
    }
}
