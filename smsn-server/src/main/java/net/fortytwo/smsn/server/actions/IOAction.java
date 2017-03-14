package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.io.Format;

import javax.validation.constraints.NotNull;
import java.io.File;

public abstract class IOAction extends FilteredAction {
    @NotNull
    private Format format;
    @NotNull
    private File file;

    public Format getFormat() {
        return notNull(format);
    }

    public void setFormat(String format) {
        this.format = Format.getFormat(format);
    }

    public File getFile() {
        return notNull(file);
    }

    public void setFile(String file) {
        this.file = new File(file);
    }
}
