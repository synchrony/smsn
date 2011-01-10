package net.fortytwo.myotherbrain.update.actions;

import net.fortytwo.myotherbrain.model.concepts.Atom;
import net.fortytwo.myotherbrain.update.UpdateException;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;

import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class SetEmphasis extends WriteAction {
    private final Float emphasis;

    private Float oldEmphasis;

    public SetEmphasis(URI subject,
                       Float emphasis,
                       final WriteContext c) throws UpdateException {
        super(subject, c);

        if (null != emphasis) {
            emphasis = c.normalizeEmphasis(emphasis);
        }

        this.emphasis = emphasis;
    }

    protected void executeUndo(final WriteContext c) throws UpdateException {
        Atom item = this.toThing(subject, Atom.class, c);
        item.setEmphasis(oldEmphasis);
    }

    protected void executeRedo(final WriteContext c) throws UpdateException {
        Atom item = this.toThing(subject, Atom.class, c);
        oldEmphasis = item.getEmphasis();
        item.setEmphasis(emphasis);
    }
}
