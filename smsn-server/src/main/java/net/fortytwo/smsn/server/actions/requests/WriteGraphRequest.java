package net.fortytwo.smsn.server.actions.requests;

import javax.validation.constraints.NotNull;

public class WriteGraphRequest extends FilteredResultsRequest {
    @NotNull
    private String format;
    @NotNull
    private String file;
    @NotNull
    private String root;
    private int height = 0;

    public String getFormat() {
        return format;
    }

    public String getFile() {
        return file;
    }

    public String getRoot() {
        return root;
    }

    public int getHeight() {
        return height;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public void setFile(String file) {
        this.file = file;
    }

    public void setRoot(String root) {
        this.root = root;
    }

    public void setHeight(int height) {
        this.height = height;
    }
}
