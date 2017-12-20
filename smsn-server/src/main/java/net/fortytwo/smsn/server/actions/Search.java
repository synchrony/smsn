package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.query.Model;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import javax.validation.constraints.NotNull;
import java.io.IOException;

/**
 * A service for executing keyword search over an Extend-o-Brain graph
 */
public class Search extends BasicViewAction {

    @NotNull
    private String query;
    @NotNull
    private Model.QueryType queryType;

    private String getQuery() {
        return notNull(query);
    }

    private Model.QueryType getQueryType() {
        return notNull(queryType);
    }

    private int titleCutoff = 100;

    public void setQuery(String query) {
        // TODO: this doesn't solve the problem (that you can't search on queries with extended characters)
        //query = new String(query.getBytes(), "UTF-8");

        this.query = query;
    }

    public void setQueryType(Model.QueryType queryType) {
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
            if (!getQueryType().equals(Model.QueryType.Ripple)) {
                addSearchResults(params);
            }
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        params.getMap().put("title", getQuery());
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
        Note tree = params.getModel().search(getQueryType(), getQuery(), height, getFilter(), style);
        addView(tree, params);
    }
}
