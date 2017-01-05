package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Note;

import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.util.List;

public class WikiWriter {
    public void toWikiText(final List<Note> notes,
                           final OutputStream out,
                           final boolean withProperties) {
        PrintStream p;
        try {
            p = new PrintStream(out, false, SemanticSynchrony.UTF8);
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException(e);
        }

        for (Note n : notes) {
            printNote(n, 0, p, withProperties);
        }
    }

    private static void printNote(final Note n,
                                  final int indent,
                                  final PrintStream p,
                                  final boolean withProperties) {
        indent(indent, p);

        p.print("* ");

        if (null != n.getId()) {
            p.print(":");
            p.print(padKey(n.getId()));
            p.print(": ");
        }

        if (null != n.getValue()) {
            p.print(escapeValue(n.getValue()));
        }

        p.print("\n");

        int nextIndent = indent + 1;

        if (withProperties) {
            if (null != n.getAlias()) printProperty(SemanticSynchrony.ALIAS, n.getAlias(), nextIndent, p);
            if (null != n.getCreated()) printProperty(SemanticSynchrony.CREATED, n.getCreated(), nextIndent, p);
            if (null != n.getPriority()) printProperty(SemanticSynchrony.PRIORITY, n.getPriority(), nextIndent, p);
            if (null != n.getSharability()) printProperty(SemanticSynchrony.SHARABILITY, n.getSharability(), nextIndent, p);
            if (null != n.getShortcut()) printProperty(SemanticSynchrony.SHORTCUT, n.getShortcut(), nextIndent, p);
            if (null != n.getWeight()) printProperty(SemanticSynchrony.WEIGHT, n.getWeight(), nextIndent, p);
        }

        for (Note child : n.getChildren()) {
            printNote(child, nextIndent, p, withProperties);
        }
    }

    private static void printProperty(final String key, final Object value, final int indent, final PrintStream p) {
        indent(indent, p);

        p.println("@" + key + " " + value);
    }

    private static void indent(final int indent, final PrintStream p) {
        for (int i = 0; i < indent; i++) {
            p.print("    ");
        }
    }

    private static String escapeValue(final String value) {
        if (value.indexOf('\n') >= 0 || value.indexOf('\r') >= 0) {
            return WikiFormat.VERBATIM_BLOCK_START + "\n" + value + "\n" + WikiFormat.VERBATIM_BLOCK_END;
        } else {
            return value;
        }
    }

    private static String padKey(String id) {
        while (id.length() < 5) {
            id = "0" + id;
        }

        return id;
    }
}
