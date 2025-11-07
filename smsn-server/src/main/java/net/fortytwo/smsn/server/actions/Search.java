package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;
import java.util.List;

/**
 * A service for executing keyword search over an Extend-o-Brain graph
 */
public class Search extends BasicViewAction {

    private String query;
    private TreeViews.QueryType queryType;

    private String getQuery() {
        return notNull(query);
    }

    private TreeViews.QueryType getQueryType() {
        return notNull(queryType);
    }

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
            if (!getQueryType().equals(TreeViews.QueryType.Ripple)) {
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
        // TODO: Migrate to use AtomRepository.search() directly
        // For now, keep using existing implementation
        TreeNode<Link> tree = params.getQueries().search(getQueryType(), getQuery(), height, getFilter(), style);
        addView(tree, params);
    }
}
