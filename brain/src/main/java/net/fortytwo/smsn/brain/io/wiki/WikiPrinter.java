package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Property;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;

import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

public class WikiPrinter {
    private final PrintStream printStream;

    public WikiPrinter(final OutputStream outputStream) {
        printStream = createPrintStream(outputStream);
    }

    public void print(final Note note) {
        printProperties(note);
        printContent(note);
    }

    private PrintStream createPrintStream(final OutputStream out) {
        try {
            return new PrintStream(out, false, SemanticSynchrony.UTF8);
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException(e);
        }
    }

    private void printInternal(final Note note,
                               final int indent,
                               final PrintStream ps) {
        indent(indent);
        printBullet(note);
        printId(note);
        printTitle(note);
        ps.print("\n");

        int nextIndent = indent + 1;

        ListNode<Note> cur = note.getFirst();
        while (null != cur) {
            printInternal(cur.getFirst(), nextIndent, ps);
            cur = cur.getRest();
        }
    }

    private void printTitle(final Note tree) {
        String label = tree.getLabel();
        if (null != label) {
            printStream.print(escapeValue(label));
        }
    }

    private void printId(final Note tree) {
        if (null != tree.getTopic()) {
            printStream.print(":");
            printStream.print(tree.getTopic().getId());
            printStream.print(": ");
        }
    }

    private void printBullet(final Note note) {
        Role role = note.getRole();
        if (null == role) {
            printStream.print(WikiFormat.NODE_BULLET);
        } else {
            switch (role) {
                case Relation:
                    printStream.print(WikiFormat.LABEL_BULLET);
                    break;
                default:
                    throw new IllegalStateException();
            }
        }
        printStream.print(" ");
    }

    private void printContent(final Note note) {
        Note cur = note.getFirst();
        while (null != cur) {
            printInternal(cur, 0, printStream);
            cur = (Note) cur.getRest();
        }
    }

    private void printProperties(final Note note) {
        printProperty("id", note.getTopic().getId());
        // TODO: label does not need to be special-cased
        printProperty("title", note.getLabel());

        Note.propertiesByKey.values().stream().filter(Property::isAnnotationProperty).forEach(prop -> {
            Object value = prop.getGetter().apply(note);
            if (null != value && (null == prop.getDefaultValue() || !value.equals(prop.getDefaultValue()))) {
                printProperty(prop.getKey(), value);
            }
        });
    }

    private void printProperty(final String key, final Object value) {
        String valueString = value.toString();
        if (0 == valueString.trim().length()) return;

        StringBuilder sb = new StringBuilder();
        sb.append("@").append(key).append(" ");
        if (containsNewline(valueString)) {
            sb.append("```\n").append(WikiFormat.stripTrailingSpace(valueString)).append("\n```");
        } else {
            sb.append(valueString);
        }
        printStream.println(sb.toString());
    }

    private boolean containsNewline(final String text) {
        return text.contains("\n");
    }

    private void indent(final int indent) {
        for (int i = 0; i < indent; i++) {
            printStream.print("    ");
        }
    }

    private static String escapeValue(final String value) {
        return value.replaceAll("\n", "\\n")
                .replaceAll("\r", "\\r");
    }
}
