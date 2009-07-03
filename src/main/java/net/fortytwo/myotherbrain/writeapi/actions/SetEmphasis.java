package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;

import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class SetEmphasis extends WriteAction {
    private final URI subject;
    private final Float emphasis;

    private Float oldEmphasis;

    public SetEmphasis(final URI subject,
                       final Float emphasis) {
        if (null == subject) {
            throw new NullPointerException();
        }

        this.subject = subject;
        this.emphasis = emphasis;
    }

    protected void executeUndo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        item.setEmphasis(oldEmphasis);
    }

    protected void executeRedo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        oldEmphasis = item.getEmphasis();
        item.setEmphasis(emphasis);
    }
}
