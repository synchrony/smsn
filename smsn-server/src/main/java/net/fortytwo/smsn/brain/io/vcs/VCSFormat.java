package net.fortytwo.smsn.brain.io.vcs;

/**
 * Shared Format instance for VCS reader and writer.
 * This ensures both use the same Format object instance so ServiceLoader can find them.
 */
public class VCSFormat {
    public static final FilePerNoteFormat FORMAT = new FilePerNoteFormat("VCS", "smsn");

    private VCSFormat() {
        // Utility class
    }
}
