package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.config.Authentication;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

abstract class FilteredAction extends Action {
    private Filter filter = Filter.noFilter();

    public Filter getFilter() {
        return notNull(filter);
    }

    public void setFilter(final Filter filter) {
        this.filter = filter;
    }

    @Override
    protected void performTransaction(final ActionContext context) {
        // Compute and store the effective filter early so it's available to subclasses
        context.setEffectiveFilter(getEffectiveFilter(context));
    }

    /**
     * Get the filter to use for this action, from the context.
     * This returns the effective filter that accounts for authentication.
     */
    protected Filter getFilterFromContext(final ActionContext context) {
        Filter effective = context.getEffectiveFilter();
        return effective != null ? effective : filter;
    }

    /**
     * Get the effective filter, taking into account authentication status.
     * For unauthenticated users, restricts to public sources only.
     */
    protected Filter getEffectiveFilter(final ActionContext context) {
        if (context.isAuthenticated()) {
            return filter;
        }

        // For anonymous users, restrict to public sources
        Authentication auth = SemanticSynchrony.getConfiguration().getAuthentication();
        if (!auth.isEnabled()) {
            return filter;
        }

        List<String> publicSources = auth.getPublicSources();
        if (publicSources == null || publicSources.isEmpty()) {
            // No public sources configured - return a filter that matches nothing
            // by setting includedSources to a non-existent source
            Set<String> noSources = new HashSet<>();
            noSources.add("__none__");
            return new Filter(
                filter.getMinWeight(),
                filter.getDefaultWeight(),
                noSources,
                filter.getDefaultSource()
            );
        }

        // Intersect requested sources with public sources
        Set<String> effectiveSources;
        Set<String> requestedSources = filter.getIncludedSources();

        if (requestedSources == null || requestedSources.isEmpty()) {
            // Client requested all sources, restrict to public only
            effectiveSources = new HashSet<>(publicSources);
        } else {
            // Client requested specific sources, intersect with public
            effectiveSources = new HashSet<>(requestedSources);
            effectiveSources.retainAll(publicSources);
            if (effectiveSources.isEmpty()) {
                // No overlap - use placeholder to match nothing
                effectiveSources.add("__none__");
            }
        }

        return new Filter(
            filter.getMinWeight(),
            filter.getDefaultWeight(),
            effectiveSources,
            filter.getDefaultSource()
        );
    }

    protected void setFilterParams(final ActionContext context) {
        Filter effectiveFilter = getEffectiveFilter(context);
        context.getMap().put(Params.DEFAULT_SOURCE, effectiveFilter.getDefaultSource());
        context.getMap().put(Params.DEFAULT_WEIGHT, effectiveFilter.getDefaultWeight());
        Set<String> includedSources = effectiveFilter.getIncludedSources();
        context.getMap().put(Params.INCLUDED_SOURCES,
            includedSources != null ? new ArrayList<>(includedSources) : new ArrayList<>());
        context.getMap().put(Params.MIN_WEIGHT, effectiveFilter.getMinWeight());
    }
}
