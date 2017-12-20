package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;

import java.util.Optional;

abstract class FilteredAction extends Action {
    private Filter filter = Filter.noFilter();

    public Filter getFilter() {
        return notNull(filter);
    }

    public void setFilter(final Filter filter) {
        this.filter = filter;
    }

    protected Note getRoot(String rootId, final ActionContext context) {
        Note root;
        if (rootId.equals(CREATE_NEW_NOTE)) {
            root = createNewRoot(context);
            rootId = Note.getId(root);
        } else {
            Optional<Note> opt = context.getBrain().getTopicGraph().getNoteById(rootId);
            if (opt.isPresent()) {
                root = opt.get();
            } else {
                throw new IllegalArgumentException("no such root: " + rootId);
            }
        }

        if (null != filter && !filter.test(root)) {
            throw new BadRequestException("root of view is not visible: " + rootId);
        }

        context.getMap().put(Params.ROOT, Note.getId(root));

        setTitle(context, null == Note.getTitle(root) || 0 == Note.getTitle(root).length()
                ? "[no title]" : Note.getTitle(root));

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
        Note.setTitle(root, "life, the universe, and everything");
        return root;
    }
}
