package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

/**
 * A service for importing a SmSn subgraph.
 * Note: This feature is not available in standalone mode.
 * Use VCS import instead.
 */
public class ReadGraph extends IOAction {

    @Override
    protected void performTransaction(final ActionContext params) throws RequestProcessingException, BadRequestException {
        throw new UnsupportedOperationException("ReadGraph is not available in standalone mode. Use VCS import instead.");
    }

    @Override
    protected boolean doesRead() {
        return false;
    }

    @Override
    protected boolean doesWrite() {
        return true;
    }
}
