package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.server.requests.RootedViewRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.security.Principal;

/**
 * A service for retrieving hierarchical views of Extend-o-Brain graphs
 */
public class GetView extends Action {

    @Override
    public String getName() {
        return "view";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams params) throws JSONException {
        ViewRequest r;

        r = new ViewRequest(request, params.getUser());

        params.setHeight(r.getHeight());
        params.setRootId(r.getRootId());
        params.setStyleName(r.getStyleName());
        params.setFilter(r.getFilter());
        params.setIncludeTypes(r.isIncludeTypes());
    }

    protected void performTransaction(final RequestParams params)
            throws RequestProcessingException, BadRequestException {

        Note note = params.getQueries().view(params.getRoot(), params.getHeight(), params.getFilter(), params.getStyle());
        try {
            addView(note, params);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        addToHistory(params.getRootId());
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
