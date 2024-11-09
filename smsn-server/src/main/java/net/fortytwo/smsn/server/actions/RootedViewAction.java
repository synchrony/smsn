package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.server.ActionContext;

public abstract class RootedViewAction extends BasicViewAction {

    private AtomId root;

    private Note rootNote;

    public Note getRoot() {
        return notNull(rootNote);
    }

    public void setRoot(String rootStr) {
        // work around for a Brain-mode quirk
        if (null != rootStr && rootStr.equals("null")) {
            this.root = null;
        } else {
            this.root = new AtomId(rootStr);
        }
    }

    @Override
    protected void performTransaction(final ActionContext context) {
        super.performTransaction(context);

        setFilterParams(context);

        if (null != root) {
            rootNote = getRoot(root, context);
            context.getMap().put(Params.ROOT, root.value);
        }
    }
}
