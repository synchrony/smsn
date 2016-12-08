package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.action.requests.RootedViewRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

public class UpdateRequest extends RootedViewRequest {
    public final String wikiView;
    public final JSONObject jsonView;

    public UpdateRequest(final JSONObject json,
                         final Principal user) throws JSONException {
        super(json, user);

        // default to wiki-formatted updates, but support updates in either the JSON or wiki formats
        String view = this.json.getString(Params.VIEW);
        String viewFormat = this.json.optString(Params.VIEW_FORMAT);
        if (null == viewFormat || 0 == viewFormat.length() || viewFormat.equals(Params.WIKI_FORMAT)) {
            wikiView = view;
            jsonView = null;
        } else if (viewFormat.equals(Params.JSON_FORMAT)) {
            wikiView = null;
            jsonView = new JSONObject(view);
        } else {
            throw new JSONException("unexpected view format: " + viewFormat);
        }
    }
}
