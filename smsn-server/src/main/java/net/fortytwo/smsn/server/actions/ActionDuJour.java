package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service to adjust a graph to data model changes
 */
public class ActionDuJour extends Action {
    @Override
    public void parseRequest(RequestParams params) throws IOException, BadRequestException {
        // nothing to do
    }

    @Override
    protected void performTransaction(RequestParams params) throws BadRequestException, RequestProcessingException {
        // nothing to do; add an "action du jour" as needed
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
