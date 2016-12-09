package net.fortytwo.smsn.server.actions.requests;

import javax.validation.constraints.NotNull;

public class UpdateRequest extends RootedViewRequest {

    public enum Format{json, wiki};

    @NotNull
    private String view;
    @NotNull
    private Format viewFormat;

    public String getView() {
        return view;
    }

    public void setView(String view) {
        this.view = view;
    }

    public Format getViewFormat() {
        return viewFormat;
    }

    public void setViewFormat(Format viewFormat) {
        this.viewFormat = viewFormat;
    }
}
