package net.fortytwo.myotherbrain.update.actions;

import net.fortytwo.myotherbrain.model.concepts.FirstClassItem;
import net.fortytwo.myotherbrain.model.concepts.WebResource;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;
import net.fortytwo.myotherbrain.update.UpdateException;

import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class SetIcon extends WriteAction {
    private final URI icon;

    private URI oldIcon;

    public SetIcon(URI subject,
                   URI icon,
                   final WriteContext c) throws UpdateException {
        super(subject, c);

        if (null != icon) {
            icon = c.normalizeResourceURI(icon);
        }
        
        this.icon = icon;
    }

    protected void executeUndo(final WriteContext c) throws UpdateException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        item.setIcon(toThing(oldIcon, WebResource.class, c));
    }

    protected void executeRedo(final WriteContext c) throws UpdateException {
        FirstClassItem subject = this.toThing(this.subject, FirstClassItem.class, c);
        oldIcon = toURI(subject.getIcon());
        subject.setIcon(toThing(icon, WebResource.class, c));
    }
}
