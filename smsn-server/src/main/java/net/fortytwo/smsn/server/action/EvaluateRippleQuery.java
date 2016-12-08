package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.server.action.requests.BasicSearchRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

/**
 * A service for executing Ripple queries over Extend-o-Brain graphs
 */
public class EvaluateRippleQuery extends Action {

    @Override
    public String getName() {
        return "ripple";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        BasicSearchRequest r = new BasicSearchRequest(request, p.getUser());

        p.setHeight(r.getHeight());
        p.setQuery(r.getQuery());
        p.setStyleName(r.getStyleName());
        p.setFilter(r.getFilter());
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        try {
            addSearchResults(p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        p.getMap().put("title", p.getQuery());
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }

    protected void addSearchResults(final RequestParams p) throws IOException {
        // TODO: restore Ripple after dealing with Android/Dalvik + dependency issues
        Note n = new Note();
        //Note n = p.queries.rippleQuery(p.query, p.depth, p.filter, p.style);
        JSONObject json;

        try {
            json = p.getWriter().toJSON(n);
        } catch (JSONException e) {
            throw new IOException(e);
        }
        p.getMap().put("view", json.toString());
    }
}
