package net.fortytwo.smsn.brain;

import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;

public class ActivityLog {
    private final OutputStreamWriter out;

    public ActivityLog(FileWriter out) {
        this.out = out;
    }

    public void logCreate(final Atom atom) {
        log("create", atom);
    }

    public void logCreateById(final AtomId atomId) {
        logById("create", atomId);
    }

    public void logView(final Atom atom) {
        log("view", atom);
    }

    public void logViewById(final AtomId atomId) {
        logById("view", atomId);
    }

    public void logSetProperties(final Atom atom) {
        log("set-props", atom);
    }

    public void logSetPropertiesById(final AtomId atomId) {
        logById("set-props", atomId);
    }

    public void logLink(final Atom tail, final Atom head) {
        log("link", tail, head);
    }

    public void logLinkById(final AtomId tailId, final AtomId headId) {
        logById("link", tailId, headId);
    }

    public void logUnlink(final Atom tail, final Atom head) {
        log("unlink", tail, head);
    }

    public void logUnlinkById(final AtomId tailId, final AtomId headId) {
        logById("unlink", tailId, headId);
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

    private void log(final String action, final Atom... atoms) {
        try {
            out.append("")
                    .append("").append(String.valueOf(System.currentTimeMillis()))
                    .append("\t").append(action);

            for (Atom atom : atoms) {
                out.append("\t").append(atom.id.value);
            }

            out.append("\n");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private void logById(final String action, final AtomId... atomIds) {
        try {
            out.append("")
                    .append("").append(String.valueOf(System.currentTimeMillis()))
                    .append("\t").append(action);

            for (AtomId atomId : atomIds) {
                out.append("\t").append(atomId.value);
            }

            out.append("\n");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
