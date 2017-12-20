package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.server.ActionContext;

import javax.validation.constraints.NotNull;

public abstract class RootedViewAction extends BasicViewAction {

    @NotNull
    private String root;

    private Note rootNote;

    public Note getRoot() {
        return notNull(rootNote);
    }

    public void setRoot(String root) {
        // work around for a Brain-mode quirk
        if (null != root && root.equals("null")) root = null;

        this.root = root;
    }

    @Override
    protected void performTransaction(final ActionContext context) {
        super.performTransaction(context);

        setFilterParams(context);

        if (null != root) {
            rootNote = getRoot(root, context);
        }
    }
}
