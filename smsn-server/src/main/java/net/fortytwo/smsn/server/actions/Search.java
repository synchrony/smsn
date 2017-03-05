package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.TreeViews;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.ActionContext;
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

    private int titleCutoff = 100;

    public void setQuery(String query) {
        // TODO: this doesn't solve the problem (that you can't search on queries with extended characters)
        //query = new String(query.getBytes(), "UTF-8");

        this.query = query;
    }

    public void setQueryType(TreeViews.QueryType queryType) {
        this.queryType = queryType;
    }

    public void setTitleCutoff(int titleCutoff) {
        this.titleCutoff = titleCutoff;
    }

    @Override
    protected void performTransaction(final ActionContext params)
            throws RequestProcessingException, BadRequestException {

        params.getJsonPrinter().setTitleLengthCutoff(titleCutoff);

        try {
            if (queryType.equals(TreeViews.QueryType.Ripple)) {
                addRippleResults(params);
            } else {
                addSearchResults(params);
            }
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        params.getMap().put("title", query);
    }

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }

    private void addSearchResults(final ActionContext params) throws IOException {
        Note n = params.getQueries().search(queryType, query, height, filter, style);
        addView(n, params);
    }

    private void addRippleResults(final ActionContext p) throws IOException {
        // TODO: restore Ripple after dealing with Android/Dalvik + dependency issues
        Note n = new Note();
        //Note n = p.queries.rippleQuery(p.query, p.depth, p.filter, style);
        JSONObject json;

        json = p.getJsonPrinter().toJson(n);
        p.getMap().put("view", json.toString());
    }
}
