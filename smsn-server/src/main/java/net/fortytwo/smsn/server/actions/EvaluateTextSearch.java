package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.actions.requests.SearchRequest;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for executing keyword search over an Extend-o-Brain graph
 */
public class EvaluateTextSearch extends Action<SearchRequest> {

    private static final int DEFAULT_VALUE_LENGTH_CUTOFF = 100;

    @Override
    public String getName() {
        return "search";
    }

    @Override
    public void parseRequest(final SearchRequest request, final RequestParams p) throws IOException {
        p.setHeight(request.getHeight());
        p.setQueryType(request.getQueryType());
        p.setQuery(request.getQuery());
        p.setStyleName(request.getStyle());
        p.setFilter(request.getFilter());
        p.setValueCutoff(request.getValueCutoff());
    }

    @Override
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

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }

    private void addSearchResults(final RequestParams params) throws IOException {
        Note n = params.getQueries().search(params.getQueryType(), params.getQuery(), params.getHeight(), params.getFilter(), params.getStyle());
        addView(n, params);
    }
}
