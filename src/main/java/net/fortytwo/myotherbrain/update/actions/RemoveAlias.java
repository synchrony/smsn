package net.fortytwo.myotherbrain.update.actions;

import net.fortytwo.myotherbrain.model.concepts.FirstClassItem;
import net.fortytwo.myotherbrain.update.UpdateException;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;
import org.openrdf.concepts.owl.Thing;

import java.net.URI;
import java.util.Set;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class RemoveAlias extends WriteAction {
    private final URI targetAlias;

    private Set<URI> beforeAlias;

    public RemoveAlias(URI subject,
                       URI targetAlias,
                       final WriteContext c) throws UpdateException {
        super(subject, c);

        if (null == targetAlias) {
            throw new NullPointerException();
        } else {
            targetAlias = c.normalizeResourceURI(targetAlias);
        }

        this.targetAlias = targetAlias;
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
        alias.remove(toThing(targetAlias, Thing.class, c));
        // Note: assumes that removal from the Set is not necessarily persisted.
        subject.setAlias(alias);
    }
}