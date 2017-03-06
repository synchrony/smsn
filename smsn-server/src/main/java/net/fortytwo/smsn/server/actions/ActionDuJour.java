package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.util.function.Function;

/**
 * A service to adjust a graph to data model changes
 */
public class ActionDuJour extends Action {

    @Override
    protected void performTransaction(ActionContext context) throws BadRequestException, RequestProcessingException {
        // nothing to do; add an "action du jour" as needed

        findAnomalousAtoms(context);
    }

    private void findAnomalousAtoms(final ActionContext context) {
        for (Atom a : context.getBrain().getTopicGraph().getAllAtoms()) {
            checkNotNull(a, Atom::getId, "id");
            checkNotNull(a, Atom::getSharability, "sharability");
            checkNotNull(a, Atom::getWeight, "weight");
            checkNotNull(a, Atom::getCreated, "created");
            checkNotNull(a, Atom::getTitle, "title");
        }
    }

    private <T> void checkNotNull(final Atom a, final Function<Atom, T> accessor,  final String name) {
        T value = accessor.apply(a);
        if (null == value) {
            System.out.println("atom " + a.getId() + " has null " + name);
        }
    }

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return true;
    }
}
