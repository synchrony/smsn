package net.fortytwo.myotherbrain.notes;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteParser {
    // Regex of valid id prefixes, including parentheses, colon and trailing space
    public static final Pattern KEY_PREFIX = Pattern.compile("[a-zA-Z0-9@&]+:");

    private static final int MAX_BULLET_LENGTH = 1;

    // Tabs count as four spaces each.
    private static final String TAB_REPLACEMENT = "    ";

    public Note parse(final String s) throws IOException, NoteParsingException {
        InputStream in = new ByteArrayInputStream(s.getBytes());
        try {
            return parse(in);
        } finally {
            in.close();
        }
    }

    public Note parse(final InputStream in) throws IOException, NoteParsingException {
        Note root = new Note();

        LinkedList<Note> hierarchy = new LinkedList<Note>();
        LinkedList<Integer> indentHierarachy = new LinkedList<Integer>();

        InputStreamReader r = new InputStreamReader(in, "UTF-8");
        BufferedReader br = new BufferedReader(r);
        String line;
        int lineNumber = 0;
        while ((line = br.readLine()) != null) {
            lineNumber++;
            //System.out.println("" + lineNumber + ") " + line);

            String l = line;

            // Tabs count as four spaces.
            l = l.replaceAll("[\\t]", TAB_REPLACEMENT);

            if (0 == l.trim().length()) {
                // empty lines are simply ignored
                continue;
            }

            // Find indent level
            int indent = 0;
            if (l.length() > 0) {
                while (' ' == l.charAt(indent)) {
                    indent++;
                }
                l = l.substring(indent);
            }

            // Extract keys
            String id = null;
            int k = l.indexOf(" ");
            if (k > 0 && KEY_PREFIX.matcher(l.substring(0, k)).matches()) {
                int i = l.indexOf(":");
                id = l.substring(0, i);

                l = l.substring(k);
                indent += k;

                k = 0;
                while (k < l.length() && ' ' == l.charAt(k)) {
                    k++;
                    indent++;
                }
                if (k > 0) {
                    l = l.substring(k);
                }
            }

            if (0 == l.length()) {
                throw new NoteParsingException(lineNumber, "missing bullet and value");
            }

            while (0 < hierarchy.size() && indentHierarachy.getLast() >= indent) {
                hierarchy.removeLast();
                indentHierarachy.removeLast();
            }

            int j = -1;
            for (int i = 0; i < l.length(); i++) {
                char c = l.charAt(i);
                if (' ' == c) {
                    j = i;
                    break;
                }
            }

            if (j < 0) {
                j = l.length();
            }

            boolean isAttribute;
            String bullet = l.substring(0, j);
            if (bullet.startsWith("@") && bullet.length() > 1) {
                isAttribute = true;
            } else {
                isAttribute = false;

                if (bullet.length() > MAX_BULLET_LENGTH) {
                    throw new NoteParsingException(lineNumber, "bullet is too long: " + bullet);
                }
            }

            // Skip white space between bullet and value
            while (j < l.length() && ' ' == l.charAt(j)) {
                j++;
            }
            l = l.substring(j);

            String value = "";
            if (0 < l.length()) {
                if (!isAttribute && l.contains("{{{")) {
                    int start = lineNumber;
                    boolean inside = false;
                    int index = 0;
                    while (true) {
                        // Check for the closing symbol before the opening symbol
                        int b2 = l.indexOf("}}}", index);
                        if (b2 >= 0) {
                            if (!inside) {
                                throw new NoteParsingException(start, "unmatched verbatim block terminator" +
                                        " (on line " + lineNumber + ")");
                            }

                            inside = false;
                            index = b2 + 3;
                            continue;
                        }

                        int b1 = l.indexOf("{{{", index);
                        if (b1 >= 0) {
                            if (inside) {
                                throw new NoteParsingException(start, "nested verbatim blocks (detected on line " +
                                        lineNumber + ") are not allowed");
                            }
                            inside = true;
                            index = b1 + 3;
                            continue;
                        }

                        value += l;

                        if (inside) {
                            value += "\n";
                            l = br.readLine();
                            if (null == l) {
                                throw new NoteParsingException(start, "non-terminated verbatim block");
                            }
                            lineNumber++;
                            index = 0;
                        } else {
                            break;
                        }
                    }
                } else {
                    value = l;
                }
            }

            value = value.trim();

            if (0 == value.length()) {
                if (isAttribute) {
                    throw new NoteParsingException(lineNumber, "empty attribute value");
                } else {
                    throw new NoteParsingException(lineNumber, "empty note");
                }
            }

            if (isAttribute) {
                Note n = 0 == hierarchy.size() ? root : hierarchy.get(hierarchy.size() - 1);

                if (bullet.equals("@alias")) {
                    if (value.length() > 0) {
                        n.setAlias(value);
                    } else {
                        throw new NoteParsingException(lineNumber, "missing @alias value");
                    }
                } else if (bullet.equals("@weight")) {
                    float val;
                    try {
                        val = Float.valueOf(value);
                    } catch (NumberFormatException e) {
                        throw new NoteParsingException(lineNumber, "invalid @weight value: " + value);
                    }
                    n.setWeight(val);
                } else if (bullet.equals("@sharability")) {
                    float val;
                    try {
                        val = Float.valueOf(value);
                    } catch (NumberFormatException e) {
                        throw new NoteParsingException(lineNumber, "invalid @sharability value: " + value);
                    }
                    n.setSharability(val);
                } else {
                    throw new NoteParsingException(lineNumber, "unknown attribute: " + bullet);
                }
            } else {
                Note n = new Note();
                n.setValue(value);

                n.setId(id);

                if (0 < hierarchy.size()) {
                    hierarchy.get(hierarchy.size() - 1).addChild(n);
                } else {
                    root.addChild(n);
                }

                hierarchy.add(n);
                indentHierarachy.add(indent);
            }
        }

        return root;
    }

    public static class NoteParsingException extends Exception {
        public NoteParsingException(final int lineNumber,
                                    final String message) {
            super("line " + lineNumber + ": " + message);
        }
    }
}
