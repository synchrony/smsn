package net.fortytwo.smsn.server.actions;

public abstract class RootedViewAction extends BasicViewAction {

    private String root;

    public String getRoot() {
        return root;
    }

    public void setRoot(String root) {
        // work around a Brain-mode quirk
        if (null != root && root.equals("null")) root = null;

        this.root = root;
    }
}
