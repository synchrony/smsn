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
public class SetName extends WriteAction {
    private final URI subject;
    private final String name;

    private String oldName;

    public SetName( URI subject,
                    String name,
                    final WriteContext c) throws WriteException {
        if (null == subject) {
            throw new NullPointerException();
        }

        this.subject = subject;
        this.name = name;
    }

    protected void executeUndo(final WriteContext c) throws WriteException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        item.setName(oldName);
    }

    protected void executeRedo(final WriteContext c) throws WriteException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        oldName = item.getName();
        item.setName(name);
    }
}