package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.query.QueryType;
import net.fortytwo.smsn.brain.view.TreeViewBuilder;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.json.JSONObject;

import java.util.List;

/**
 * A service for executing keyword search over an Extend-o-Brain graph
 */
public class Search extends BasicViewAction {

    private String query;
    private QueryType queryType;

    private String getQuery() {
        return notNull(query);
    }

    private QueryType getQueryType() {
        return notNull(queryType);
    }

    private int titleCutoff = 100;

    public void setQuery(String query) {
        // TODO: this doesn't solve the problem (that you can't search on queries with extended characters)
        //query = new String(query.getBytes(), "UTF-8");

        this.query = query;
    }

    public void setQueryType(QueryType queryType) {
        this.queryType = queryType;
    }

    public void setTitleCutoff(int titleCutoff) {
        this.titleCutoff = titleCutoff;
    }

    @Override
    protected void performTransaction(final ActionContext params)
            throws RequestProcessingException, BadRequestException {

        params.getJsonPrinter().setTitleLengthCutoff(titleCutoff);

        if (!getQueryType().equals(QueryType.Ripple)) {
            addSearchResults(params);
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

    private void addSearchResults(final ActionContext params) {
        // Use AtomRepository.search() and TreeViewBuilder
        List<Atom> results = params.getRepository().search(getQuery(), getFilter());

        TreeViewBuilder builder = new TreeViewBuilder(params.getRepository());
        net.fortytwo.smsn.brain.TreeNode tree = builder.buildSearchResultsView(results, height, getFilter());

        // Serialize directly using new JSON printer
        try {
            JSONObject json = params.getTreeNodeJsonPrinter().toJson(tree);
            params.getMap().put(net.fortytwo.smsn.brain.Params.VIEW, json);
        } catch (java.io.IOException e) {
            throw new RequestProcessingException(e);
        }
    }
}
