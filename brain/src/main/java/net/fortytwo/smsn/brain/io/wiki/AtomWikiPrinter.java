package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;

import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

/**
 * Prints Atom objects in Wiki format for .smsn files.
 * This replaces the old WikiPrinter which worked with Page/TreeNode<Link>.
 */
public class AtomWikiPrinter {
    private final PrintStream printStream;

    public AtomWikiPrinter(final OutputStream outputStream) {
        printStream = createPrintStream(outputStream);
    }

    public void print(final Atom atom) {
        printProperties(atom);
        printChildren(atom);
    }

    private PrintStream createPrintStream(final OutputStream out) {
        try {
            return new PrintStream(out, false, SemanticSynchrony.UTF8);
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException(e);
        }
    }

    private void printProperties(final Atom atom) {
        // Required properties
        printProperty("id", atom.id.value);
        printProperty("title", atom.title);
        printProperty("created", atom.created.value);
        printProperty("weight", atom.weight.value);
        printProperty("source", atom.source.value);

        // Optional properties
        if (atom.priority.isPresent()) {
            printProperty("priority", atom.priority.get().value);
        }

        if (atom.alias.isPresent()) {
            printProperty("alias", atom.alias.get());
        }

        if (atom.shortcut.isPresent()) {
            printProperty("shortcut", atom.shortcut.get());
        }

        if (atom.text.isPresent()) {
            printProperty("text", atom.text.get());
        }
    }

    private void printChildren(final Atom atom) {
        if (atom.children != null && !atom.children.isEmpty()) {
            for (net.fortytwo.smsn.brain.AtomId childId : atom.children) {
                // Print child reference in wiki format: "* :childId:"
                printStream.println(WikiFormat.NODE_BULLET + " :" + childId.value + ":");
            }
        }
    }

    private void printProperty(final String key, final Object value) {
        String valueString = value.toString();
        if (valueString.trim().isEmpty()) return;

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
}
