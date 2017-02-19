package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for removing isolated atoms (i.e. atoms with neither parents nor children) from an Extend-o-Brain graph
 */
public class RemoveIsolatedAtoms extends FilteredAction {

    @Override
    public void parseRequest(final RequestParams params) throws IOException {
        params.setFilter(getFilter());
    }

    @Override
    protected void performTransaction(final RequestParams params) throws RequestProcessingException, BadRequestException {
        params.getBrain().getTopicGraph().removeIsolatedAtoms(params.getFilter());
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
