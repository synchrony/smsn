package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for finding isolated atoms (i.e. atoms with no parents or children) in an Extend-o-Brain graph
 */
public class FindIsolatedAtoms extends FilteredAction {

    @Override
    protected void performTransaction(final ActionContext context) throws RequestProcessingException {
        Note n = context.getQueries().findIsolatedAtoms(getFilter());
        try {
            addView(n, context);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        context.getMap().put("title", "isolated atoms");
    }

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }
}
