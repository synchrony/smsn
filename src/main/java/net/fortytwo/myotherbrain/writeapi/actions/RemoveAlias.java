package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.undo.UndoableAction;

import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class RemoveAlias extends UndoableAction {
    private final URI subject;
    private final URI alias;

    public RemoveAlias(final URI subject,
                       final URI alias) {
        this.subject = subject;
        this.alias = alias;
    }

    protected void executeUndo(Object t) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    protected void executeRedo(Object t) {
        //To change body of implemented methods use File | Settings | File Templates.
    }
}