package net.fortytwo.smsn.brain.io.markdown;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;

import java.io.File;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * A printer for the special Markdown format used to make human-readable pages out of SmSn data.
 * Note that this is unrelated to the formats which are read by <code>MarkdownParser</code>.
 */
public class MarkdownPrinter {
    private final PrintStream printStream;
    private final Map<String, File> dirs;

    public MarkdownPrinter(OutputStream outputStream,
                           Map<String, File> dirs) {
        printStream = createPrintStream(outputStream);
        this.dirs = dirs;
    }

    public void print(final Note note) {
        AtomId id = Note.getId(note);
        Long created = Note.getCreated(note);
        Float weight = Note.getWeight(note);
        Float priority = Note.getPriority(note);
        String title = Note.getTitle(note);
        String text = Note.getText(note);
        String alias = Note.getAlias(note);

        List<Note> children = new ArrayList<>();
        ListNode<Note> cur = note.getChildren();
        while (cur != null) {
            children.add(cur.getFirst());
            cur = cur.getRest();
        }

        printStream.println("# " + title + "\n");

        printStream.println("id: " + id.value);

        printStream.println("created: " + toUtcDateTimeString(created));

        if (weight != null && weight > 0f) {
            printStream.println("weight: " + weight);
        }

        if (priority != null && priority > 0f) {
            printStream.println("priority: " + priority);
        }

        if (alias != null) {
            printStream.println("alias: [" + alias + "](" + alias + ")");
        }

        if (!children.isEmpty()) {
            printStream.println("\nchildren:");
            for (Note child : children) {
                printStream.println("* " + toChildReference(note, child));
            }
        }

        if (text != null) {
            printStream.println("\n```");
            printStream.println(text);
            printStream.println("```");
        }
    }

    public static String toUtcDateTimeString(long millis) {
        return Instant.ofEpochMilli(millis)
                .atOffset(ZoneOffset.UTC)
                .format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS 'UTC'"));
    }

    public String toChildReference(Note parent, Note child) {
        String id = Note.getId(child).value;

        String title = Note.getTitle(child);
        if (title == null || title.isEmpty()) {
            title = "[unavailable]";
        }

        String parentSource = Note.getSource(parent);
        String childSource = Note.getSource(child);

        if (childSource == null) {
            return title;
        } else {
            File parentDir = dirs.get(parentSource);
            File childDir = dirs.get(childSource);
            String rel = parentDir.toPath().relativize(childDir.toPath()).toString();
            String url = (rel.isEmpty() ? "" : rel + "/") + id + ".md";

            return "[" + title + "](" + url + ")";
        }
    }

    private PrintStream createPrintStream(final OutputStream out) {
        try {
            return new PrintStream(out, false, SemanticSynchrony.UTF8);
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException(e);
        }
    }
}
