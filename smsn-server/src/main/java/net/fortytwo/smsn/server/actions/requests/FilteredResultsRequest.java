package net.fortytwo.smsn.server.actions.requests;

import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.server.Request;

import javax.validation.constraints.NotNull;

public class FilteredResultsRequest extends Request {
    @NotNull
    private Filter filter;

    public Filter getFilter() {
        return filter;
    }

    public void setFilter(final Filter filter) {
        this.filter = filter;
    }
}
