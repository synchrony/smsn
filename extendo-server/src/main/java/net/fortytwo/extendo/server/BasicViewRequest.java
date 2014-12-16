package net.fortytwo.extendo.server;

import net.fortytwo.extendo.brain.Params;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
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
