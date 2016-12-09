package net.fortytwo.smsn.server.actions.requests;

public class SearchRequest extends BasicSearchRequest {
    private int valueCutoff;

    public int getValueCutoff() {
        return valueCutoff;
    }

    public void setValueCutoff(int valueCutoff) {
        this.valueCutoff = valueCutoff;
    }
}
