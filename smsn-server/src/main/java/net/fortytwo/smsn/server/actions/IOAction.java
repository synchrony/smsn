package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.io.Format;

import javax.validation.constraints.NotNull;
import java.io.File;

public abstract class IOAction extends FilteredAction {
    @NotNull
    protected Format format;
    @NotNull
    protected File file;

    public void setFormat(String format) {
        this.format = Format.getFormat(format);
    }

    public void setFile(String file) {
        this.file = new File(file);
    }
}
