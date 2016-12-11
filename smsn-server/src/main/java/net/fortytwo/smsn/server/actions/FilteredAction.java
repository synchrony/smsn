package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.server.Action;

import javax.validation.constraints.NotNull;

abstract class FilteredAction extends Action {
    @NotNull
    private Filter filter;

    public Filter getFilter() {
        return filter;
    }

    public void setFilter(final Filter filter) {
        this.filter = filter;
    }
}
