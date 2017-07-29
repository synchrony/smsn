package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Property;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;

import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

public class WikiPrinter {
    private final PrintStream printStream;

    public WikiPrinter(final OutputStream outputStream) {
        printStream = createPrintStream(outputStream);
    }

    public void print(final Page page) {
        printProperties(page);
        printContent(page);
        printText(page);
    }

    private PrintStream createPrintStream(final OutputStream out) {
        try {
            return new PrintStream(out, false, SemanticSynchrony.UTF8);
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException(e);
        }
    }

    private void printInternal(final TreeNode<Link> tree,
                               final int indent,
                               final PrintStream ps) {
        indent(indent);
        printBullet(tree);
        printId(tree);
        printTitle(tree);
        ps.print("\n");

        int nextIndent = indent + 1;

        ListNode<TreeNode<Link>> cur = tree.getChildren();
        while (null != cur) {
            printInternal(cur.getFirst(), nextIndent, ps);
            cur = cur.getRest();
        }
    }

    private void printTitle(final TreeNode<Link> tree) {
        if (null != tree.getValue().getLabel()) {
            printStream.print(escapeValue(tree.getValue().getLabel()));
        }
    }

    private void printId(final TreeNode<Link> tree) {
        if (null != tree.getValue().getTarget()) {
            printStream.print(":");
            printStream.print(tree.getValue().getTarget().getId());
            printStream.print(": ");
        }
    }

    private void printBullet(final TreeNode<Link> tree) {
        Role role = tree.getValue().getRole();
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

    private void printContent(final Page page) {
        TreeNode<Link> content = page.getContent();
        if (null != content) {
            ListNode<TreeNode<Link>> cur = content.getChildren();
            while (null != cur) {
                printInternal(cur.getFirst(), 0, printStream);
                cur = cur.getRest();
            }
        }
    }

    private void printText(final Page page) {
        if (!isEmptyText(page.getText())) {
            printStream.println("@text");
            printStream.print(page.getText());
        }
    }

    private boolean isEmptyText(final String text) {
        return null == text || text.trim().length() == 0;
    }

    private void printProperties(final Page page) {
        printProperty("id", page.getContent().getValue().getTarget().getId());
        printProperty("title", page.getContent().getValue().getLabel());

        Page.propertiesByKey.values().stream().filter(Property::isAnnotationProperty).forEach(prop -> {
            Object value = prop.getGetter().apply(page);
            if (null != value) {
                printProperty(prop.getPropertyKey(), value);
            }
        });
    }

    private void printProperty(final String key, final Object value) {
        printStream.println("@" + key + " " + value);
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
