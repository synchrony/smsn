package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

/**
 * A service for deriving a prioritized list of items in the knowledge base.
 * Note: This feature is not available in standalone mode.
 */
public class GetPriorities extends FilteredAction {

    private int maxResults = 100;

    public void setMaxResults(int maxResults) {
        if (maxResults <= 0) {
            throw new IllegalArgumentException(Params.MAX_RESULTS + " parameter must be a positive integer");
        }

        this.maxResults = maxResults;
    }

    @Override
    protected void performTransaction(final ActionContext context) throws RequestProcessingException, BadRequestException {
        throw new UnsupportedOperationException("GetPriorities is not available in standalone mode");
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
