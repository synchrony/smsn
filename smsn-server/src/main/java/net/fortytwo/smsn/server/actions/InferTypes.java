package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

/**
 * A service for type inference.
 * Note: This feature is not available in standalone mode.
 */
public class InferTypes extends Action {

    @Override
    protected void performTransaction(final ActionContext params) throws RequestProcessingException, BadRequestException {
        throw new UnsupportedOperationException("InferTypes is not available in standalone mode");
    }

    @Override
    protected boolean doesRead() {
        return false;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }
}
