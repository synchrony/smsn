package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.TreeViews;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.json.JSONObject;

import javax.validation.constraints.NotNull;
import java.io.IOException;

/**
 * A service for executing keyword search over an Extend-o-Brain graph
 */
public class Search extends BasicViewAction {

    @NotNull
    private String query;
    @NotNull
    private TreeViews.QueryType queryType;

    private int valueCutoff = 100;

    public TreeViews.QueryType getQueryType() {
        return queryType;
    }

    public String getQuery() {
        return query;
    }

    public void setQuery(String query) {
        // TODO: this doesn't solve the problem (that you can't search on queries with extended characters)
        //query = new String(query.getBytes(), "UTF-8");

        this.query = query;
    }

    public void setQueryType(TreeViews.QueryType queryType) {
        this.queryType = queryType;
    }

    public int getValueCutoff() {
        return valueCutoff;
    }

    public void setValueCutoff(int valueCutoff) {
        this.valueCutoff = valueCutoff;
    }

    @Override
    public void parseRequest(final RequestParams params) throws IOException {
        params.setHeight(getHeight());
        params.setQueryType(getQueryType());
        params.setQuery(getQuery());
        params.setStyleName(getStyle());
        params.setFilter(getFilter());
        params.setValueCutoff(getValueCutoff());
    }

    @Override
    protected void performTransaction(final RequestParams params) throws RequestProcessingException, BadRequestException {
        params.getJsonPrinter().setTitleLengthCutoff(params.getValueCutoff());

        try {
            if (params.getQueryType().equals(TreeViews.QueryType.Ripple)) {
                addRippleResults(params);
            } else {
                addSearchResults(params);
            }
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        params.getMap().put("title", params.getQuery());
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

    private void addRippleResults(final RequestParams p) throws IOException {
        // TODO: restore Ripple after dealing with Android/Dalvik + dependency issues
        Note n = new Note();
        //Note n = p.queries.rippleQuery(p.query, p.depth, p.filter, p.style);
        JSONObject json;

        json = p.getJsonPrinter().toJson(n);
        p.getMap().put("view", json.toString());
    }
}
