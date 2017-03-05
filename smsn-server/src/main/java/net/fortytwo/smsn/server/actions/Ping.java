package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.ActionContext;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

/**
 * A minimal "ping" service
 */
public class Ping extends Action {

    @Override
    protected void performTransaction(final ActionContext params) throws RequestProcessingException, BadRequestException {
        params.getMap().put("time", String.valueOf(System.currentTimeMillis()));
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
