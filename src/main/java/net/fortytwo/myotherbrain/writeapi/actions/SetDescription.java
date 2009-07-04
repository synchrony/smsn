package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.writeapi.WriteAction;
import net.fortytwo.myotherbrain.writeapi.WriteContext;
import net.fortytwo.myotherbrain.writeapi.WriteException;

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

    public SetDescription(URI subject,
                          String description,
                          final WriteContext c) throws WriteException {
        if (null == subject) {
            throw new NullPointerException();
        } else {
            description = c.normalizeDescription(description);
        }

        this.subject = subject;
        this.description = description;
    }

    protected void executeUndo(final WriteContext c) throws WriteException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        item.setDescription(oldDescription);
    }

    protected void executeRedo(final WriteContext c) throws WriteException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        oldDescription = item.getDescription();
        item.setDescription(description);
    }
}
