package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.server.ActionContext;

import javax.validation.constraints.NotNull;

public abstract class RootedViewAction extends BasicViewAction {

    @NotNull
    private String root;

    private Atom rootAtom;

    public Atom getRoot() {
        return notNull(rootAtom);
    }

    public void setRoot(String root) {
        // work around for a Brain-mode quirk
        if (null != root && root.equals("null")) root = null;

        this.root = root;
    }

    @Override
    protected void performTransaction(final ActionContext params) {
        super.performTransaction(params);

        if (null != root) {
            rootAtom = getRoot(root, params);
        }
    }
}
