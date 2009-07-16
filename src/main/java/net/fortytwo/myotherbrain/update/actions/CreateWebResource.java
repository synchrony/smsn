package net.fortytwo.myotherbrain.update.actions;

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
public class CreateWebResource extends WriteAction {

    protected final String representationMediaType;
    protected final String representationSha1Sum;

    public CreateWebResource(
            URI subject,
            String representationMediaType,
            String representationSha1Sum,
            final WriteContext c) throws UpdateException {
        super(subject, c);

        if (null == representationMediaType) {
            representationMediaType = c.normalizeRepresentationMediaType(representationMediaType);
        }

        if (null == representationSha1Sum) {
            representationSha1Sum = c.normalizeRepresentationSha1Sum(representationSha1Sum);
        }

        this.representationMediaType = representationMediaType;
        this.representationSha1Sum = representationSha1Sum;
    }

    protected void executeUndo(final WriteContext c) throws UpdateException {
        WebResource subject = toThing(this.subject, WebResource.class, c);
        c.remove(subject);
    }

    protected void executeRedo(final WriteContext c) throws UpdateException {
        // TODO: is there any reason to use "designate" over "create"?
        WebResource subject = c.designate(toQName(this.subject), WebResource.class);
        subject.setRepresentationMediaType(representationMediaType);
        subject.setRepresentationSha1Sum(representationSha1Sum);
    }
}