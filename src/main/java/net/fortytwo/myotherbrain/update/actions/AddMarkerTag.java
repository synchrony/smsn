package net.fortytwo.myotherbrain.update.actions;

import net.fortytwo.myotherbrain.model.concepts.FirstClassItem;
import net.fortytwo.myotherbrain.model.concepts.Marker;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;
import net.fortytwo.myotherbrain.update.UpdateException;

import java.net.URI;
import java.util.Set;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class AddMarkerTag extends WriteAction {
    private final URI newMarkerTag;

    private Set<URI> beforeMarkerTag;

    public AddMarkerTag(URI subject,
                        URI newMarkerTag,
                        final WriteContext c) throws UpdateException {
        super(subject, c);

        if (null == newMarkerTag) {
            throw new NullPointerException();
        } else {
            newMarkerTag = c.normalizeResourceURI(newMarkerTag);
        }

        this.newMarkerTag = newMarkerTag;
    }

    protected void executeUndo(final WriteContext c) throws UpdateException {
        FirstClassItem subject = this.toThing(this.subject, FirstClassItem.class, c);
        subject.setMarkerTag(toThingSet(beforeMarkerTag, Marker.class, c));
    }

    protected void executeRedo(final WriteContext c) throws UpdateException {
        FirstClassItem subject = this.toThing(this.subject, FirstClassItem.class, c);
        Set<Marker> markerTag = subject.getMarkerTag();
        beforeMarkerTag = toURISet(markerTag);

        // Note: assumes that the returned Set is modifiable.
        markerTag.add(toThing(newMarkerTag, Marker.class, c));
        // Note: assumes that items added to the Set are not necessarily persisted.
        subject.setMarkerTag(markerTag);
    }
}
