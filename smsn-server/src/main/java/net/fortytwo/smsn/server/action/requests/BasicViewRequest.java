package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.brain.Params;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

public class BasicViewRequest extends FilteredResultsRequest {
    private final int height;
    private final String styleName;

    public BasicViewRequest(final JSONObject json,
                            final Principal user) throws JSONException {
        super(json, user);

        height = this.json.getInt(Params.HEIGHT);
        styleName = this.json.getString(Params.STYLE);
    }

    public int getHeight() {
        return height;
    }

    public String getStyleName() {
        return styleName;
    }
}
