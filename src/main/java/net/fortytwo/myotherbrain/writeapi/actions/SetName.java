package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;

import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class SetName extends WriteAction {
    private final URI subject;
    private final String name;

    private String oldName;

    public SetName(final URI subject,
                   final String name) {
        if (null == subject) {
            throw new NullPointerException();
        }

        this.subject = subject;
        this.name = name;
    }

    protected void executeUndo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        item.setName(oldName);
    }

    protected void executeRedo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        oldName = item.getName();
        item.setName(name);
    }
}