package net.fortytwo.myotherbrain.update.actions;

import net.fortytwo.myotherbrain.model.concepts.FirstClassItem;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;
import net.fortytwo.myotherbrain.update.UpdateException;

import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class SetDescription extends WriteAction {
    private final String description;
    private final String richTextDescription;

    private String oldDescription;
    private String oldRichTextDescription;

    public SetDescription(URI subject,
                          String description,
                          String richTextDescription,
                          final WriteContext c) throws UpdateException {
        super(subject, c);

        if (null != description) {
            description = c.normalizeDescription(description);
        }

        if (null != richTextDescription) {
            richTextDescription = c.normalizeRichTextDescription(richTextDescription);
        }

        this.description = description;
        this.richTextDescription = richTextDescription;
    }

    protected void executeUndo(final WriteContext c) throws UpdateException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        item.setDescription(oldDescription);
        item.setRichTextDescription(oldRichTextDescription);
    }

    protected void executeRedo(final WriteContext c) throws UpdateException {
        FirstClassItem item = this.toThing(subject, FirstClassItem.class, c);
        oldDescription = item.getDescription();
        oldRichTextDescription = item.getRichTextDescription();
        item.setDescription(description);
        item.setRichTextDescription(richTextDescription);
    }
}
