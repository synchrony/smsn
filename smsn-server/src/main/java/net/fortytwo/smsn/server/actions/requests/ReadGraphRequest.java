package net.fortytwo.smsn.server.actions.requests;

import net.fortytwo.smsn.server.Request;

import javax.validation.constraints.NotNull;

public class ReadGraphRequest extends Request {
    @NotNull
    private String format;
    @NotNull
    private String file;

    public String getFormat() {
        return format;
    }

    public String getFile() {
        return file;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public void setFile(String file) {
        this.file = file;
    }
}
