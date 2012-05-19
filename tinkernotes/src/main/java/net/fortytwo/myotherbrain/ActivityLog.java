package net.fortytwo.myotherbrain;

import java.io.FileWriter;
import java.io.IOException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ActivityLog {
    private final FileWriter out;

    public ActivityLog(FileWriter out) {
        this.out = out;
    }

    public void logCreate(final Atom a) {
        log(a, "create");
    }

    public void logView(final Atom a) {
        log(a, "view");
    }

    public void logUpdate(final Atom a) {
        log(a, "change-value");
    }

    public void logSetProperties(final Atom a) {
        log(a, "set-props");
    }

    public void flush() {
        try {
            out.flush();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void shutDown() throws IOException {
        out.close();
    }

    private void log(final Atom a,
                     final String action) {
        try {
            out.append("")
                    .append("" + System.currentTimeMillis())
                    .append("\t")
                    .append(a.asVertex().getId().toString())
                    .append("\t").append(action)
                    .append("\n");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
