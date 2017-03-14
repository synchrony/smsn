package net.fortytwo.smsn.server.actions;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;

import javax.validation.constraints.NotNull;

abstract class FilteredAction extends Action {
    @NotNull
    private Filter filter;

    public Filter getFilter() {
        return notNull(filter);
    }

    public void setFilter(final Filter filter) {
        this.filter = filter;
    }

    protected Atom getRoot(final String rootId, final ActionContext context) {
        Atom root = rootId.equals(CREATE_NEW_ATOM)
                ? createNewRoot(context)
                : context.getBrain().getTopicGraph().getAtomById(rootId);

        if (null != filter && !filter.isVisible(root)) {
            throw new BadRequestException("root of view is not visible: " + rootId);
        }

        context.getMap().put(Params.ROOT, root.getId());

        setTitle(context, null == root.getTitle() || 0 == root.getTitle().length()
                ? "[no title]" : root.getTitle());

        return root;
    }

    private Atom createNewRoot(final ActionContext context) {
        Atom root = context.getBrain().getTopicGraph().createAtomWithProperties(getFilter(), null);
        root.setTitle("life, the universe, and everything");
        return root;
    }
}
