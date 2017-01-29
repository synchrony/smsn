package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.NoteQueries;

public abstract class BasicViewAction extends FilteredAction {
    private int height;

    private String style = NoteQueries.forwardViewStyle.getName();

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
