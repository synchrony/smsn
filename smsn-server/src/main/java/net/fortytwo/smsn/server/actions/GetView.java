package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for retrieving hierarchical views of Extend-o-Brain graphs
 */
public class GetView extends RootedViewAction {

    @Override
    protected void performTransaction(final ActionContext conte)
            throws RequestProcessingException, BadRequestException {
        super.performTransaction(conte);

        Note note = conte.getQueries().view(rootAtom, height, filter, style);
        try {
            addView(note, conte);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        addToHistory(root);
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
