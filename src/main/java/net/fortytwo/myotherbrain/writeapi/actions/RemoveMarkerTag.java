package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.model.beans.Marker;
import net.fortytwo.myotherbrain.writeapi.WriteAction;
import net.fortytwo.myotherbrain.writeapi.WriteContext;
import net.fortytwo.myotherbrain.writeapi.WriteException;

import java.net.URI;
import java.util.Set;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class RemoveMarkerTag extends WriteAction {
    private final URI subject;
    private final URI targetTag;

    private Set<URI> beforeMarkerTag;

    public RemoveMarkerTag(URI subject,
                           URI targetTag,
                           final WriteContext c) throws WriteException {
        if (null == subject) {
            throw new NullPointerException();
        } else {
            subject = c.normalizeResourceURI(subject);
        }

        if (null == targetTag) {
            throw new NullPointerException();
        } else {
            targetTag = c.normalizeResourceURI(targetTag);
        }

        this.subject = subject;
        this.targetTag = targetTag;
    }

    protected void executeUndo(final WriteContext c) throws WriteException {
        FirstClassItem subject = this.toThing(this.subject, FirstClassItem.class, c);
        subject.setMarkerTag(toThingSet(beforeMarkerTag, Marker.class, c));
    }

    protected void executeRedo(final WriteContext c) throws WriteException {
        FirstClassItem subject = this.toThing(this.subject, FirstClassItem.class, c);
        Set<Marker> markerTag = subject.getMarkerTag();
        beforeMarkerTag = toURISet(markerTag);

        // Note: assumes that the returned Set is modifiable.
        markerTag.remove(toThing(targetTag, Marker.class, c));
        // Note: assumes that removal from the Set is not necessarily persisted.
        subject.setMarkerTag(markerTag);
    }
}
