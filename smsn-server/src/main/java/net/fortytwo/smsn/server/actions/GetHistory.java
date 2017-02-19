package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for finding recently visited atoms
 */
public class GetHistory extends FilteredAction {

    @Override
    public void parseRequest(final RequestParams params) throws IOException {
        params.setFilter(getFilter());
    }

    @Override
    protected void performTransaction(final RequestParams params) throws RequestProcessingException, BadRequestException {
        Iterable<Atom> atoms = getHistory(params.getBrain().getTopicGraph(), params.getFilter());

        try {
            addView(params.getQueries().customView(atoms, params.getFilter()), params);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
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
