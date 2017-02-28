package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A no-op
 */
public class NoAction extends Action {
    @Override
    public void parseRequest(RequestParams params) throws IOException, BadRequestException {
        // nothing to do
    }

    @Override
    protected void performTransaction(RequestParams params) throws BadRequestException, RequestProcessingException {
        // nothing to do
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
