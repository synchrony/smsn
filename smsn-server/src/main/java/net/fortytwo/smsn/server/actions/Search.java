package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.NoteQueries;
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
    private NoteQueries.QueryType queryType;

    private int valueCutoff = 100;

    public NoteQueries.QueryType getQueryType() {
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

    public void setQueryType(NoteQueries.QueryType queryType) {
        this.queryType = queryType;
    }

    public int getValueCutoff() {
        return valueCutoff;
    }

    public void setValueCutoff(int valueCutoff) {
        this.valueCutoff = valueCutoff;
    }

    @Override
    public void parseRequest(final RequestParams p) throws IOException {
        p.setHeight(getHeight());
        p.setQueryType(getQueryType());
        p.setQuery(getQuery());
        p.setStyleName(getStyle());
        p.setFilter(getFilter());
        p.setValueCutoff(getValueCutoff());
    }

    @Override
    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        p.getJsonWriter().setValueLengthCutoff(p.getValueCutoff());

        try {
            if (p.getQueryType().equals(NoteQueries.QueryType.Ripple)) {
                addRippleResults(p);
            } else {
                addSearchResults(p);
            }
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

    private void addRippleResults(final RequestParams p) throws IOException {
        // TODO: restore Ripple after dealing with Android/Dalvik + dependency issues
        Note n = new Note();
        //Note n = p.queries.rippleQuery(p.query, p.depth, p.filter, p.style);
        JSONObject json;

        json = p.getJsonWriter().toJson(n);
        p.getMap().put("view", json.toString());
    }
}
