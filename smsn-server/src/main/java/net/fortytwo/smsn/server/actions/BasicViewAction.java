package net.fortytwo.smsn.server.actions;

import javax.validation.constraints.NotNull;

public abstract class BasicViewAction extends FilteredAction {
    private int height;
    @NotNull
    private String style;

    public BasicViewAction() {
        super();
    }

    public int getHeight() {
        return height;
    }

    public String getStyle() {
        return style;
    }

    public void setHeight(int height) {
        this.height = height;
    }

    public void setStyle(String style) {
        this.style = style;
    }
}
