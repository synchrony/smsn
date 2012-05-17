package net.fortytwo.myotherbrain.notes;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NotesSyntax {
    // Regex of valid id prefixes, including parentheses, colon and trailing space
    public static final Pattern KEY_PREFIX = Pattern.compile("[a-zA-Z0-9@&]+:");

    private static final int MAX_BULLET_LENGTH = 2;

    // Tabs count as four spaces each.
    private static final String TAB_REPLACEMENT = "    ";

    public void writeNotes(final List<Note> notes,
                           final OutputStream out) {
        PrintStream p;
        try {
            p = new PrintStream(out, false, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException(e);
        }

        for (Note n : notes) {
            printNote(n, 0, p);
        }
    }

    public JSONObject toJSON(final Note n) throws JSONException {
        JSONObject json = new JSONObject();

        JSONObject link = new JSONObject();
        json.put("link", link);

        JSONObject target = new JSONObject();
        json.put("target", target);
        target.put("key", n.getId());
        target.put("weight", n.getWeight());
        target.put("sharability", n.getSharability());
        target.put("value", n.getValue());
        target.put("created", n.getCreated());

        if (0 < n.getChildren().size()) {
            JSONArray c = new JSONArray();
            json.put("children", c);
            int i = 0;
            for (Note child : n.getChildren()) {
                c.put(i, toJSON(child));
                i++;
            }
        }

        return json;
    }

    public List<Note> readNotes(final InputStream in) throws IOException, NoteParsingException {
        List<Note> notes = new LinkedList<Note>();
        parseInternal(in, notes);
        return notes;
    }

    private void parseInternal(final InputStream in,
                               final Collection<Note> notes) throws IOException, NoteParsingException {
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
            String targetKey = null;
            int k = l.indexOf(" ");
            if (k > 0 && KEY_PREFIX.matcher(l.substring(0, k)).matches()) {
                int i = l.indexOf(":");
                targetKey = l.substring(0, i);

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
                throw new NoteParsingException(lineNumber, "missing key value");
            }

            while (0 < hierarchy.size() && indentHierarachy.getLast() >= indent) {
                hierarchy.removeLast();
                indentHierarachy.removeLast();
            }

            boolean esc = false;
            int j = -1;
            for (int i = 0; i < l.length(); i++) {
                char c = l.charAt(i);
                if (' ' == c) {
                    if (!esc) {
                        j = i;
                        break;
                    }
                } else esc = '\\' == c && !esc;
            }

            if (j < 0) {
                j = l.length();
            }

            String bullet = l.substring(0, j);
            if (bullet.length() > MAX_BULLET_LENGTH) {
                throw new NoteParsingException(lineNumber, "bullet is too long: " + bullet);
            }
            while (j < l.length() && ' ' == l.charAt(j)) {
                j++;
            }
            l = l.substring(j);

            String value = "";
            if (0 < l.length()) {
                if (l.contains("{{{")) {
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
                throw new NoteParsingException(lineNumber, "empty note");
            }

            Note n = new Note();
            n.setValue(value);

            n.setId(targetKey);

            if (0 < hierarchy.size()) {
                hierarchy.get(hierarchy.size() - 1).addChild(n);
            } else {
                notes.add(n);
            }

            hierarchy.add(n);
            indentHierarachy.add(indent);

        }
    }

    public class NoteParsingException extends Exception {
        public NoteParsingException(final int lineNumber,
                                    final String message) {
            super("line " + lineNumber + ": " + message);
        }
    }

    private static boolean isValidValue(final String value) {
        for (char c : value.toCharArray()) {
            if (Character.isISOControl(c)) {
                return false;
            }
        }

        return true;
    }

    private static String sanitizeValue(final String value) {
        return null == value || 0 == value.length() || !isValidValue(value)
                ? "???"
                : value;
    }

    private static void printNote(final Note n,
                                  final int indent,
                                  final PrintStream p) {

        if (null != n.getId()) {
            if (null != n.getId()) {
                p.print(padKey(n.getId()));
            }
            p.print(": ");
        }

        for (int i = 0; i < indent; i++) {
            p.print("    ");
        }

        p.print("* ");

        p.print(sanitizeValue(n.getValue()));

        p.print("\n");

        for (Note child : n.getChildren()) {
            printNote(child, indent + 1, p);
        }
    }

    private static String padKey(String id) {
        while (id.length() < 5) {
            id = "0" + id;
        }

        return id;
    }
}
