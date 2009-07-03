package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;

import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class SetDescription extends WriteAction {
    private final URI subject;
    private final String description;

    private String oldDescription;

    public SetDescription(final URI subject,
                          final String description) {
        if (null == subject) {
            throw new NullPointerException();
        }

        this.subject = subject;
        this.description = description;
    }

    protected void executeUndo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem item = this.toEntity(subject, FirstClassItem.class, c);
        item.setDescription(oldDescription);
    }

    protected void executeRedo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem item = this.toEntity(subject, FirstClassItem.class, c);
        oldDescription = item.getDescription();
        item.setDescription(description);
    }
}
