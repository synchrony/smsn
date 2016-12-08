package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * A minimal "ping" service
 */
public class Ping extends Action {

    @Override
    public String getName() {
        return "ping";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        p.getMap().put("time", String.valueOf(System.currentTimeMillis()));
    }

    protected boolean doesRead() {
        return false;
    }

    protected boolean doesWrite() {
        return false;
    }
}
