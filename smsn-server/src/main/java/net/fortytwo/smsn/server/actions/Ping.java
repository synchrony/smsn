package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.Request;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

/**
 * A minimal "ping" service
 */
public class Ping extends Action<Request> {

    @Override
    public String getName() {
        return "ping";
    }

    @Override
    public void parseRequest(final Request request, final RequestParams p) {
    }

    @Override
    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        p.getMap().put("time", String.valueOf(System.currentTimeMillis()));
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
