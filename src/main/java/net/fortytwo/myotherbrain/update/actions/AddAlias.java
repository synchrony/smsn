package net.fortytwo.myotherbrain.update.actions;

import net.fortytwo.myotherbrain.model.concepts.FirstClassItem;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;
import net.fortytwo.myotherbrain.update.UpdateException;
import org.openrdf.concepts.owl.Thing;

import java.net.URI;
import java.util.Set;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class AddAlias extends WriteAction {
    private final URI newAlias;

    private Set<URI> beforeAlias;

    public AddAlias(URI subject,
                    URI newAlias,
                    final WriteContext c) throws UpdateException {
        super(subject, c);

        if (null == newAlias) {
            throw new NullPointerException();
        } else {
            newAlias = c.normalizeResourceURI(newAlias);
        }

        this.newAlias = newAlias;
    }

    protected void executeUndo(final WriteContext c) throws UpdateException {
        FirstClassItem subject = this.toThing(this.subject, FirstClassItem.class, c);
        subject.setAlias(toThingSet(beforeAlias, Thing.class, c));
    }

    protected void executeRedo(final WriteContext c) throws UpdateException {
        FirstClassItem subject = this.toThing(this.subject, FirstClassItem.class, c);
        Set<Thing> alias = subject.getAlias();
        beforeAlias = toURISet(alias);

        // Note: assumes that the returned Set is modifiable.
        alias.add(toThing(newAlias, Thing.class, c));
        // Note: assumes that items added to the Set are not necessarily persisted.
        subject.setAlias(alias);
    }
}
