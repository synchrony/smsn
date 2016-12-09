package net.fortytwo.smsn.server.actions.requests;

public class RootedViewRequest extends BasicViewRequest {

    private String root;

    public String getRoot() {
        return root;
    }

    public void setRoot(String root) {
        // work around a Brain-mode quirk
        if (root.equals("null")) root = null;

        this.root = root;
    }
}
