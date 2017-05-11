package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Note;

import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

public class WikiPrinter {

    private boolean useCanonicalFormat;

    public void setUseCanonicalFormat(boolean useCanonicalFormat) {
        this.useCanonicalFormat = useCanonicalFormat;
    }

    public void print(final Note note,
                      final OutputStream out,
                      final boolean withProperties) {

        printInternal(note, 0, createPrintStream(out), withProperties);
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
                               final PrintStream p,
                               final boolean withProperties) {
        indent(indent, p);

        p.print("* ");

        if (null != note.getId()) {
            p.print(":");
            p.print(note.getId());
            p.print(": ");
        }

        if (null != note.getTitle()) {
            p.print(escapeValue(note.getTitle()));
        }

        p.print("\n");

        int nextIndent = indent + 1;

        if (withProperties) {
            printProperties(note, p, nextIndent);
        }

        for (Note child : note.getChildren()) {
            printInternal(child, nextIndent, p, withProperties);
        }

        if (useCanonicalFormat && !isEmptyPage(note.getPage())) {
            p.println();
            p.print(note.getPage());
        }
    }

    private boolean isEmptyPage(final String page) {
        return null == page || page.trim().length() == 0;
    }

    private static void printProperties(final Note n, final PrintStream p, final int indent) {
        for (Note.Property prop : Note.propertiesByKey.values()) {
            if (prop.isAnnotationProperty()) {
                Object value = prop.getNoteGetter().apply(n);
                if (null != value) {
                    indent(indent, p);
                    p.println("@" + prop.getPropertyKey() + " " + value);
                }
            }
        }
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
}
