package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.actions.requests.FilteredResultsRequest;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for removing isolated atoms (i.e. atoms with neither parents nor children) from an Extend-o-Brain graph
 */
public class RemoveIsolatedAtoms extends Action<FilteredResultsRequest> {

    @Override
    public String getName() {
        return "remove-isolated-atoms";
    }

    @Override
    public void parseRequest(final FilteredResultsRequest request, final RequestParams p) throws IOException {
        p.setFilter(request.getFilter());
    }

    @Override
    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        p.getBrain().getAtomGraph().removeIsolatedAtoms(p.getFilter());
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
