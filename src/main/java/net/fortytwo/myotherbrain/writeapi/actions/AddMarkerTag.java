package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.model.beans.Marker;

import java.net.URI;
import java.util.Set;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class AddMarkerTag extends WriteAction {
    private final URI subject;
    private final URI newMarkerTag;

    private Set<URI> beforeMarkerTag;

    public AddMarkerTag(final URI subject,
                        final URI newMarkerTag) {
        if (null == subject) {
            throw new NullPointerException();
        }

        if (null == newMarkerTag) {
            throw new NullPointerException();
        }

        this.subject = subject;
        this.newMarkerTag = newMarkerTag;
    }

    protected void executeUndo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem subject = this.toThing(this.subject, FirstClassItem.class, c);
        subject.setMarkerTag(toThingSet(beforeMarkerTag, Marker.class, c));
    }

    protected void executeRedo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem subject = this.toThing(this.subject, FirstClassItem.class, c);
        Set<Marker> markerTag = subject.getMarkerTag();
        beforeMarkerTag = toURISet(markerTag);

        // Note: assumes that the returned Set is modifiable.
        markerTag.add(toThing(newMarkerTag, Marker.class, c));
        // Note: assumes that items added to the Set are not necessarily persisted.
        subject.setMarkerTag(markerTag);
    }
}
