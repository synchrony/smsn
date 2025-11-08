package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.view.ViewDirection;
import net.fortytwo.smsn.server.ActionContext;

public abstract class RootedViewAction extends BasicViewAction {

    private AtomId root;
    private String style;

    private Atom rootAtom;
    private ViewDirection viewDirection = ViewDirection.FORWARD; // default

    public Atom getRoot() {
        return notNull(rootAtom);
    }

    public ViewDirection getViewDirection() {
        return viewDirection;
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

    public void setStyle(String style) {
        this.style = style;
        // Parse style string to ViewDirection
        if (style != null) {
            if (style.equalsIgnoreCase("backward")) {
                this.viewDirection = ViewDirection.BACKWARD;
            } else {
                // "forward", "forward-add-only", or any other style defaults to forward
                this.viewDirection = ViewDirection.FORWARD;
            }
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
