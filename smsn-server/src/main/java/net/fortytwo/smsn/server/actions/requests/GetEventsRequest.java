package net.fortytwo.smsn.server.actions.requests;

import net.fortytwo.smsn.server.Request;

public class GetEventsRequest extends Request {
    private int height;

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }
}
