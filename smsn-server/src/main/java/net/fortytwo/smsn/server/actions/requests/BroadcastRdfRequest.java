package net.fortytwo.smsn.server.actions.requests;

import net.fortytwo.smsn.server.Request;

import javax.validation.constraints.NotNull;

public class BroadcastRdfRequest extends Request {

    @NotNull
    private String dataset;

    public String getDataset() {
        return dataset;
    }

    public void setDataset(String dataset) {
        this.dataset = dataset;
    }
}
