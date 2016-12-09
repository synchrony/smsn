package net.fortytwo.smsn.server.actions.requests;

import javax.validation.constraints.NotNull;

public class BasicViewRequest extends FilteredResultsRequest {
    private int height;
    @NotNull
    private String style;

    public BasicViewRequest() {
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
