package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Tag;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;

import javax.validation.constraints.NotNull;
import java.util.Optional;

abstract class FilteredAction extends Action {
    @NotNull
    private Filter filter = Filter.noFilter();

    public Filter getFilter() {
        return notNull(filter);
    }

    public void setFilter(final Filter filter) {
        this.filter = filter;
    }

    protected Note getRoot(String rootId, final ActionContext context) {
        Note root;
        if (rootId.equals(CREATE_NEW_ATOM)) {
            root = createNewRoot(context);
            rootId = root.getId();
        } else {
            Optional<Note> opt = context.getBrain().getTopicGraph().getNotesById(rootId);
            if (opt.isPresent()) {
                root = opt.get();
            } else {
                throw new IllegalArgumentException("no such root: " + rootId);
            }
        }

        if (null != filter && !filter.test(root)) {
            throw new BadRequestException("root of view is not visible: " + rootId);
        }

        context.getMap().put(Params.ROOT, root.getId());

        setTitle(context, null == root.getTitle() || 0 == root.getTitle().length()
                ? "[no title]" : root.getTitle());

        return root;
    }

    protected void setFilterParams(final ActionContext context) {
        context.getMap().put(Params.DEFAULT_SOURCE, filter.getDefaultSource());
        context.getMap().put(Params.DEFAULT_WEIGHT, filter.getDefaultWeight());
        context.getMap().put(Params.MIN_SOURCE, filter.getMinSource());
        context.getMap().put(Params.MIN_WEIGHT, filter.getMinWeight());
    }

    private Note createNewRoot(final ActionContext context) {
        Note root = context.getBrain().getTopicGraph().createNoteWithProperties(getFilter(), null);
        root.setTitle("life, the universe, and everything");
        return root;
    }
}
