package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.model.beans.WebResource;

import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class SetIcon extends WriteAction {
    private final URI subject;
    private final URI icon;

    private URI oldIcon;

    public SetIcon(final URI subject,
                   final URI icon) {
        if (null == subject) {
            throw new NullPointerException();
        }

        this.subject = subject;
        this.icon = icon;
    }

    protected void executeUndo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        item.setIcon(toThing(oldIcon, WebResource.class, c));
    }

    protected void executeRedo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem subject = this.toThing(this.subject, FirstClassItem.class, c);
        oldIcon = toURI(subject.getIcon());
        subject.setIcon(toThing(icon, WebResource.class, c));
    }
}
