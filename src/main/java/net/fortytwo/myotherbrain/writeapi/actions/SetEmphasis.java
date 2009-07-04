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
public class SetEmphasis extends WriteAction {
    private final URI subject;
    private final Float emphasis;

    private Float oldEmphasis;

    public SetEmphasis(URI subject,
                       Float emphasis,
                       final WriteContext c) throws WriteException {
        if (null == subject) {
            throw new NullPointerException();
        } else {
            subject = c.normalizeResourceURI(subject);
        }

        if (null != emphasis) {
            emphasis = c.normalizeEmphasis(emphasis);
        }

        this.subject = subject;
        this.emphasis = emphasis;
    }

    protected void executeUndo(final WriteContext c) throws WriteException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        item.setEmphasis(oldEmphasis);
    }

    protected void executeRedo(final WriteContext c) throws WriteException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        oldEmphasis = item.getEmphasis();
        item.setEmphasis(emphasis);
    }
}
