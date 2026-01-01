package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;

import java.util.ArrayList;
import java.util.Set;

abstract class FilteredAction extends Action {
    private Filter filter = Filter.noFilter();

    public Filter getFilter() {
        return notNull(filter);
    }

    public void setFilter(final Filter filter) {
        this.filter = filter;
    }

    protected void setFilterParams(final ActionContext context) {
        context.getMap().put(Params.DEFAULT_SOURCE, filter.getDefaultSource());
        context.getMap().put(Params.DEFAULT_WEIGHT, filter.getDefaultWeight());
        Set<String> includedSources = filter.getIncludedSources();
        context.getMap().put(Params.INCLUDED_SOURCES,
            includedSources != null ? new ArrayList<>(includedSources) : new ArrayList<>());
        context.getMap().put(Params.MIN_WEIGHT, filter.getMinWeight());
    }
}
