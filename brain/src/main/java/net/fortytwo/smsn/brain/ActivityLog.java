package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;

import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;

public class ActivityLog {
    private final OutputStreamWriter out;

    public ActivityLog(FileWriter out) {
        this.out = out;
    }

    public void logUpdate(final Topic topic) {
        log("update", topic);
    }

    public void logView(final Topic topic) {
        log("view", topic);
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

    private void log(final String action, final Topic... topics) {
        try {
            out.append("")
                    .append("").append(String.valueOf(System.currentTimeMillis()))
                    .append("\t").append(action);

            for (Topic topic : topics) {
                out.append("\t").append(topic.getId());
            }

            out.append("\n");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
