package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.server.requests.RootedViewRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.security.Principal;

/**
 * A service for retrieving hierarchical views of Extend-o-Brain graphs
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GetView extends Action {

    @Override
    public String getName() {
        return "view";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {
        ViewRequest r;

        r = new ViewRequest(request, p.user);

        p.height = r.getHeight();
        p.rootId = r.getRootId();
        p.styleName = r.getStyleName();
        p.filter = r.getFilter();
        p.includeTypes = r.isIncludeTypes();
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {

        Note n = p.queries.view(p.root, p.height, p.filter, p.style);
        try {
            addView(n, p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        addToHistory(p.rootId);
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }

    private class ViewRequest extends RootedViewRequest {

        private final boolean includeTypes;

        public ViewRequest(final JSONObject json,
                           final Principal user) throws JSONException {
            super(json, user);

            // this argument is optional; do not include types by default
            includeTypes = json.optBoolean(Params.INCLUDE_TYPES, false);
        }

        public boolean isIncludeTypes() {
            return includeTypes;
        }
    }
}
