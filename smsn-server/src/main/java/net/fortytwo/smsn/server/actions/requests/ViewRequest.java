package net.fortytwo.smsn.server.actions.requests;

public class ViewRequest extends RootedViewRequest {

    private boolean includeTypes = false;

    public boolean isIncludeTypes() {
        return includeTypes;
    }

    public void setIncludeTypes(boolean includeTypes) {
        this.includeTypes = includeTypes;
    }
}
