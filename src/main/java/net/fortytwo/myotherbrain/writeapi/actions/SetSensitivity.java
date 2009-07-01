package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.undo.UndoableAction;
import net.fortytwo.myotherbrain.MOBModelConnection;

import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class SetSensitivity extends UndoableAction<MOBModelConnection> {
    private final URI subject;
    private final URI sensitivity;

    public SetSensitivity(final URI subject,
                        final URI sensitivity) {
        this.subject = subject;
        this.sensitivity = sensitivity;
    }

    protected void executeUndo(MOBModelConnection t) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    protected void executeRedo(MOBModelConnection t) {
        //To change body of implemented methods use File | Settings | File Templates.
    }
}