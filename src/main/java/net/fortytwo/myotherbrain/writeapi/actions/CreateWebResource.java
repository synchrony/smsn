package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.model.beans.WebResource;
import net.fortytwo.myotherbrain.writeapi.WriteAction;
import net.fortytwo.myotherbrain.writeapi.WriteContext;
import net.fortytwo.myotherbrain.writeapi.WriteException;

import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class CreateWebResource extends WriteAction {

    protected final String representationMediaType;
    protected final String representationSha1Sum;

    public CreateWebResource(
            URI subject,
            String representationMediaType,
            String representationSha1Sum,
            final WriteContext c) throws WriteException {
        super(subject, c);

        if (null == representationMediaType) {
            throw new NullPointerException();
        } else {
            representationMediaType = c.normalizeRepresentationMediaType(representationMediaType);
        }

        if (null == representationSha1Sum) {
            throw new NullPointerException();
        } else {
            representationSha1Sum = c.normalizeRepresentationSha1Sum(representationSha1Sum);
        }

        this.representationMediaType = representationMediaType;
        this.representationSha1Sum = representationSha1Sum;
    }

    protected void executeUndo(final WriteContext c) throws WriteException {
        WebResource subject = toThing(this.subject, WebResource.class, c);
        c.remove(subject);
    }

    protected void executeRedo(final WriteContext c) throws WriteException {
        // TODO: is there any reason to use "designate" over "create"?
        WebResource subject = c.designate(toQName(this.subject), WebResource.class);
        subject.setRepresentationMediaType(representationMediaType);
        subject.setRepresentationSha1Sum(representationSha1Sum);
    }
}