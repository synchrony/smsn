package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.entities.Note;

import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;

public class ActivityLog {
    private final OutputStreamWriter out;

    public ActivityLog(FileWriter out) {
        this.out = out;
    }

    public void logCreate(final Note a) {
        log("create", a);
    }

    public void logView(final Note a) {
        log("view", a);
    }

    public void logSetProperties(final Note a) {
        log("set-props", a);
    }

    public void logLink(final Note tail,
                        final Note head) {
        log("link", tail, head);
    }

    public void logUnlink(final Note tail,
                          final Note head) {
        log("unlink", tail, head);
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

    private void log(final String action, final Note... notes) {
        try {
            out.append("")
                    .append("").append(String.valueOf(System.currentTimeMillis()))
                    .append("\t").append(action);

            for (Note note : notes) {
                out.append("\t").append(Note.getId(note).value);
            }

            out.append("\n");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
