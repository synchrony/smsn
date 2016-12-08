package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.action.requests.SearchRequest;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

/**
 * A service for executing keyword search over an Extend-o-Brain graph
 */
public class EvaluateTextSearch extends Action {

    private static final int DEFAULT_VALUE_LENGTH_CUTOFF = 100;

    @Override
    public String getName() {
        return "search";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        SearchRequest r = new SearchRequest(request, p.getUser());

        p.setHeight(r.getHeight());
        p.setQueryType(r.getQueryType());
        p.setQuery(r.getQuery());
        p.setStyleName(r.getStyleName());
        p.setFilter(r.getFilter());
        p.setValueCutoff(r.valueCutoff);
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        if (null == p.getValueCutoff()) {
            p.getWriter().setValueLengthCutoff(DEFAULT_VALUE_LENGTH_CUTOFF);
        } else {
            p.getWriter().setValueLengthCutoff(p.getValueCutoff());
        }

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

    protected void addSearchResults(final RequestParams params) throws IOException {
        Note n = params.getQueries().search(params.getQueryType(), params.getQuery(), params.getHeight(), params.getFilter(), params.getStyle());
        addView(n, params);
    }

}
