package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import org.openrdf.concepts.owl.Thing;

import java.net.URI;
import java.util.Set;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class AddAlias extends WriteAction {
    private final URI subject;
    private final URI newAlias;

    private Set<URI> beforeAlias;

    public AddAlias(final URI subject,
                    final URI newAlias) {
        if (null == subject) {
            throw new NullPointerException();
        }

        if (null == newAlias) {
            throw new NullPointerException();
        }

        this.subject = subject;
        this.newAlias = newAlias;
    }

    protected void executeUndo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem subject = this.toThing(this.subject, FirstClassItem.class, c);
        subject.setAlias(toThingSet(beforeAlias, Thing.class, c));
    }

    protected void executeRedo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem subject = this.toThing(this.subject, FirstClassItem.class, c);
        Set<Thing> alias = subject.getAlias();
        beforeAlias = toURISet(alias);

        // Note: assumes that the returned Set is modifiable.
        alias.add(toThing(newAlias, Thing.class, c));
        // Note: assumes that items added to the Set are not necessarily persisted.
        subject.setAlias(alias);
    }
}
