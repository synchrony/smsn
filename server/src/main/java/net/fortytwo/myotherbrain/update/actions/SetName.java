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
public class SetName extends WriteAction {
    private final String name;

    private String oldName;

    public SetName(URI subject,
                   String name,
                   final WriteContext c) throws UpdateException {
        super(subject, c);

        if (null != name) {
            name = c.normalizeName(name);
        }

        this.name = name;
    }

    protected void executeUndo(final WriteContext c) throws UpdateException {
        Atom item = this.toThing(subject, Atom.class, c);
        item.setName(oldName);
    }

    protected void executeRedo(final WriteContext c) throws UpdateException {
        Atom item = this.toThing(subject, Atom.class, c);
        oldName = item.getName();
        item.setName(name);
    }
}