package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.actions.requests.FilteredResultsRequest;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for finding recently visited atoms
 */
public class GetHistory extends Action<FilteredResultsRequest> {

    @Override
    public String getName() {
        return "history";
    }

    @Override
    public void parseRequest(final FilteredResultsRequest request, final RequestParams p) throws IOException {
        p.setFilter(request.getFilter());
    }

    @Override
    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        Iterable<Atom> atoms = getHistory(p.getBrain().getAtomGraph(), p.getFilter());

        try {
            addView(p.getQueries().customView(atoms, p.getFilter()), p);
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
