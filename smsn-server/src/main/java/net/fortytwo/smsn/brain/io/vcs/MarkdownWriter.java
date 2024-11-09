package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.brain.io.markdown.MarkdownPrinter;

public class MarkdownWriter extends FilePerNoteWriter {
    public static FilePerNoteFormat FORMAT = new FilePerNoteFormat("Markdown", "md");

    public MarkdownWriter() {
        super(FORMAT,
                (note, out) -> new MarkdownPrinter(out, FilePerNoteFormat.directoriesBySource()).print(note));
    }
}
