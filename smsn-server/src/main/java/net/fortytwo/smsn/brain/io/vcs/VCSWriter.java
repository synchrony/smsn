package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.brain.io.wiki.WikiPrinter;

public class VCSWriter extends FilePerNoteWriter {
    public static final FilePerNoteFormat FORMAT = new FilePerNoteFormat("VCS", "smsn");

    public VCSWriter() {
        super(FORMAT, (note, out) -> new WikiPrinter(out).print(noteToPage(note)));
    }
}
