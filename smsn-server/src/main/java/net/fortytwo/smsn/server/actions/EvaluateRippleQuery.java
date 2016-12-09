package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import net.fortytwo.smsn.server.actions.requests.BasicSearchRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

/**
 * A service for executing Ripple queries over Extend-o-Brain graphs
 */
public class EvaluateRippleQuery extends Action<BasicSearchRequest> {

    @Override
    public String getName() {
        return "ripple";
    }

    @Override
    public void parseRequest(final BasicSearchRequest request, final RequestParams p) throws IOException {
        p.setHeight(request.getHeight());
        p.setQuery(request.getQuery());
        p.setStyleName(request.getStyle());
        p.setFilter(request.getFilter());
    }

    @Override
    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        try {
            addSearchResults(p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        p.getMap().put("title", p.getQuery());
    }

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }

    private void addSearchResults(final RequestParams p) throws IOException {
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
