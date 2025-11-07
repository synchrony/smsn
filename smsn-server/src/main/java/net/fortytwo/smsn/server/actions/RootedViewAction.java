package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.ActionContext;

public abstract class RootedViewAction extends BasicViewAction {

    private AtomId root;

    private Atom rootAtom;

    public Atom getRoot() {
        return notNull(rootAtom);
    }

    public AtomId getRootId() {
        return root;
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
            rootAtom = getRootAtom(root, context);
            context.getMap().put(Params.ROOT, root.value);

            // Set title from atom
            String title = rootAtom.title;
            setTitle(context, (null == title || 0 == title.length()) ? "[no title]" : title);
        }
    }

    private Atom getRootAtom(AtomId rootId, final ActionContext context) {
        // TODO: Handle CREATE_NEW_NOTE case when needed
        Atom atom = context.getRepository().load(rootId);

        if (null != getFilter() && !context.getRepository().testFilter(atom, getFilter())) {
            throw new net.fortytwo.smsn.server.errors.BadRequestException("root of view is not visible: " + rootId.value);
        }

        return atom;
    }
}
