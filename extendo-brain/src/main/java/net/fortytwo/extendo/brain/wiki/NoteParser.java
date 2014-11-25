package net.fortytwo.extendo.brain.wiki;

import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Note;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteParser {

    // regex of valid id suffixes
    public static final Pattern
            ID = Pattern.compile("[a-zA-Z0-9-_]+"),
            ID_SUFFIX = Pattern.compile(":[a-zA-Z0-9-_]+:");

    private static final String
            ALIAS_ATTR = "@alias",
            PRIORITY_ATTR = "@priority",
            SHARABILITY_ATTR = "@sharability",
            SHORTCUT_ATTR = "@shortcut",
            WEIGHT_ATTR = "@weight";

    private static final int MAX_BULLET_LENGTH = 1;

    // Tabs count as four spaces each.
    private static final String TAB_REPLACEMENT = "    ";

    public Note fromWikiText(final String s) throws IOException, NoteParsingException {
        InputStream in = new ByteArrayInputStream(s.getBytes(Extendo.UTF8));
        try {
            return fromWikiText(in);
        } finally {
            in.close();
        }
    }

    public Note fromWikiText(final InputStream in) throws IOException, NoteParsingException {
        Note root = new Note();

        LinkedList<Note> hierarchy = new LinkedList<Note>();
        LinkedList<Integer> indentHierarachy = new LinkedList<Integer>();

        InputStreamReader r = new InputStreamReader(in, Extendo.UTF8);
        BufferedReader br = new BufferedReader(r);
        String line;
        int lineNumber = 0;
        while ((line = br.readLine()) != null) {
            lineNumber++;

            String l = line;

            // Tabs count as four spaces each.
            l = l.replaceAll("[\\t]", TAB_REPLACEMENT);

            if (0 == l.trim().length()) {
                // Empty lines are simply ignored.
                continue;
            }

            if (l.endsWith(NoteWriter.VALUE_TRUNCATOR)) {
                throw new NoteParsingException(lineNumber,
                        "line ends with the reserved truncation sequence \"" + NoteWriter.VALUE_TRUNCATOR + "\"");
            }

            // find indent level
            int indent = 0;
            if (l.length() > 0) {
                while (' ' == l.charAt(indent)) {
                    indent++;
                }
                l = l.substring(indent);
            }

            if (0 == l.length()) {
                throw new NoteParsingException(lineNumber, "missing bullet and value");
            }

            while (0 < hierarchy.size() && indentHierarachy.getLast() >= indent) {
                hierarchy.removeLast();
                indentHierarachy.removeLast();
            }

            // parse bullet or attribute name
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

            // skip white space between bullet and value
            while (j < l.length() && ' ' == l.charAt(j)) {
                j++;
            }
            l = l.substring(j);

            // find id, if present
            String id = null;
            if (!isAttribute) {
                Matcher m = ID_SUFFIX.matcher(l);
                if (m.find() && 0 == m.start()) {
                    id = l.substring(1, m.end() - 1);
                    l = l.substring(m.end()).trim();
                }
            }

            String value = "";
            if (0 < l.length()) {
                if (!isAttribute && l.contains("{{{")) {
                    int start = lineNumber;
                    boolean inside = true;
                    int index = l.indexOf("{{{") + 3;
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
                    // can "clear" alias or shortcut by writing "@alias" or "@shortcut" and nothing else;
                    // all other attributes require an argument
                    if (!(bullet.equals(ALIAS_ATTR) || bullet.equals(SHORTCUT_ATTR))) {
                        throw new NoteParsingException(lineNumber, "empty attribute value");
                    }
                } else if (null == id) {
                    throw new NoteParsingException(lineNumber, "empty value for new note");
                } else {
                    // Empty note values are allowed for existing notes.
                    // They signify that an existing note's value should not be overwritten.
                    value = null;
                }
            }

            if (isAttribute) {
                Note n = 0 == hierarchy.size() ? root : hierarchy.get(hierarchy.size() - 1);

                if (bullet.equals(ALIAS_ATTR)) {
                    if (value.length() > 0) {
                        n.setAlias(value);
                    } else {
                        n.setAlias(Note.CLEARME_VALUE);
                    }
                } else if (bullet.equals(SHORTCUT_ATTR)) {
                    if (value.length() > 0) {
                        n.setShortcut(value);
                    } else {
                        n.setShortcut(Note.CLEARME_VALUE);
                    }
                } else if (bullet.equals(PRIORITY_ATTR)) {
                    float val;
                    try {
                        val = Float.valueOf(value);
                    } catch (NumberFormatException e) {
                        throw new NoteParsingException(lineNumber, "invalid @priority value: " + value);
                    }
                    n.setPriority(val);
                } else if (bullet.equals(SHARABILITY_ATTR)) {
                    float val;
                    try {
                        val = Float.valueOf(value);
                    } catch (NumberFormatException e) {
                        throw new NoteParsingException(lineNumber, "invalid @sharability value: " + value);
                    }
                    n.setSharability(val);
                } else if (bullet.equals(WEIGHT_ATTR)) {
                    float val;
                    try {
                        val = Float.valueOf(value);
                    } catch (NumberFormatException e) {
                        throw new NoteParsingException(lineNumber, "invalid @weight value: " + value);
                    }
                    n.setWeight(val);
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

    public Note fromJSON(final JSONObject j) throws JSONException {
        Note n = new Note();

        if (j.has(NoteWriter.ID)) {
            n.setId(j.getString(NoteWriter.ID));
        }
        if (j.has(Extendo.VALUE)) {
            n.setValue(j.getString(Extendo.VALUE));
        }
        if (j.has(Extendo.ALIAS)) {
            n.setAlias(j.getString(Extendo.ALIAS));
        }
        if (j.has(Extendo.SHORTCUT)) {
            n.setShortcut(j.getString(Extendo.SHORTCUT));
        }
        if (j.has(Extendo.SHARABILITY)) {
            n.setSharability((float) j.getDouble(Extendo.SHARABILITY));
        }
        if (j.has(Extendo.WEIGHT)) {
            n.setWeight((float) j.getDouble(Extendo.WEIGHT));
        }
        if (j.has(Extendo.PRIORITY)) {
            n.setPriority((float) j.getDouble(Extendo.PRIORITY));
        }
        if (j.has(Extendo.CREATED)) {
            n.setCreated(j.getLong(Extendo.CREATED));
        }
        if (j.has(NoteWriter.HAS_CHILDREN)) {
            n.setHasChildren(j.optBoolean(NoteWriter.HAS_CHILDREN));
        }

        JSONArray a = j.optJSONArray(NoteWriter.CHILDREN);
        if (null != a) {
            for (int i = 0; i < a.length(); i++) {
                JSONObject jc = a.getJSONObject(i);
                n.addChild(fromJSON(jc));
            }
        }

        return n;
    }

    public static class NoteParsingException extends Exception {
        public NoteParsingException(final int lineNumber,
                                    final String message) {
            super("line " + lineNumber + ": " + message);
        }
    }
}
