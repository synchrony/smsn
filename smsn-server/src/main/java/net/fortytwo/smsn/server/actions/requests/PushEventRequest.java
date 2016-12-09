package net.fortytwo.smsn.server.actions.requests;

import net.fortytwo.smsn.server.Request;

public class PushEventRequest extends Request {
    private String view;

    public String getView() {
        return view;
    }

    public void setView(String view) {
        this.view = view;
    }
}
