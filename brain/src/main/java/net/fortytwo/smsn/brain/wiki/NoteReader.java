package net.fortytwo.smsn.brain.wiki;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Note;
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

public class NoteReader {

    // regex of valid id suffixes
    public static final Pattern ID = Pattern.compile("[a-zA-Z0-9-_]+");
    private static final Pattern ID_SUFFIX = Pattern.compile(":[a-zA-Z0-9-_]+:");

    public static final String VERBATIM_BLOCK_START = "{{{";
    public static final String VERBATIM_BLOCK_END = "}}}";

    private static final int MAX_BULLET_LENGTH = 1;

    // Tabs count as four spaces each.
    private static final String TAB_REPLACEMENT = "    ";

    public Note fromWikiText(final String s) throws IOException, NoteParsingException {
        try (InputStream in = new ByteArrayInputStream(s.getBytes(SemanticSynchrony.UTF8))) {
            return fromWikiText(in);
        }
    }

    public Note fromWikiText(final InputStream in) throws IOException, NoteParsingException {
        Note root = new Note();

        LinkedList<Note> hierarchy = new LinkedList<>();
        LinkedList<Integer> indentHierarachy = new LinkedList<>();

        InputStreamReader r = new InputStreamReader(in, SemanticSynchrony.UTF8);
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

            // parse bullet or property name
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

            boolean isProperty;
            String bullet = l.substring(0, j);
            if (bullet.startsWith("@") && bullet.length() > 1) {
                isProperty = true;
            } else {
                isProperty = false;

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
            if (!isProperty) {
                Matcher m = ID_SUFFIX.matcher(l);
                if (m.find() && 0 == m.start()) {
                    id = l.substring(1, m.end() - 1);
                    l = l.substring(m.end()).trim();
                }
            }

            String value = "";
            if (0 < l.length()) {
                String lt = l.trim();
                if (!isProperty && lt.startsWith(VERBATIM_BLOCK_START)) {
                    if (lt.length() > 3) {
                        throw new NoteParsingException(lineNumber, "verbatim block must open with a line containing only '{{{'");
                    }

                    StringBuilder verbatimValue = new StringBuilder();
                    boolean first = true;
                    while (true) {
                        String nextLine = br.readLine();
                        lineNumber++;
                        if (nextLine.contains(VERBATIM_BLOCK_END)) {
                            lt = nextLine.trim();
                            if (lt.length() > 3) {
                                throw new NoteParsingException(lineNumber, "verbatim block must close with a line containing only '}}}'");
                            } else {
                                break;
                            }
                        } else {
                            if (first) {
                                first = false;
                            } else {
                                verbatimValue.append('\n');
                            }
                            verbatimValue.append(nextLine);
                        }
                    }

                    value = verbatimValue.toString();
                } else {
                    value = l;
                }
            }

            value = value.trim();

            if (0 == value.length()) {
                if (isProperty) {
                    // can "clear" alias or shortcut by writing "@alias" or "@shortcut" and nothing else;
                    // all other properties require an argument
                    if (!(bullet.equals("@alias") || bullet.equals("@shortcut"))) {
                        throw new NoteParsingException(
                                lineNumber, "empty value for property candidate '" + bullet + "'");
                    }
                } else if (null == id) {
                    throw new NoteParsingException(lineNumber, "empty value for new note");
                } else {
                    // Empty note values are allowed for existing notes.
                    // They signify that an existing note's value should not be overwritten.
                    value = null;
                }
            }

            if (isProperty) {
                Note n = 0 == hierarchy.size() ? root : hierarchy.get(hierarchy.size() - 1);

                String key = bullet.substring(1);
                parseProperty(n, key, value, lineNumber);
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

    private float getFloat(final String key, final String value, final int lineNumber) throws NoteParsingException {
        try {
            return Float.valueOf(value);
        } catch (NumberFormatException e) {
            throw new NoteParsingException(lineNumber, "invalid @" + key + " value: " + value);
        }
    }

    private long getLong(final String key, final String value, final int lineNumber) throws NoteParsingException {
        try {
            return Long.valueOf(value);
        } catch (NumberFormatException e) {
            throw new NoteParsingException(lineNumber, "invalid @" + key + " value: " + value);
        }
    }

    private void parseProperty(final Note note, final String key, final String value, int lineNumber)
            throws NoteParsingException {

        switch (key) {
            case SemanticSynchrony.ALIAS:
                if (value.length() > 0) {
                    note.setAlias(value);
                } else {
                    note.setAlias(Note.CLEARME_VALUE);
                }
                break;
            case SemanticSynchrony.CREATED:
                note.setCreated(getLong(key, value, lineNumber));
                break;
            case SemanticSynchrony.PRIORITY: {
                note.setPriority(getFloat(key, value, lineNumber));
                break;
            }
            case SemanticSynchrony.SHARABILITY: {
                note.setSharability(getFloat(key, value, lineNumber));
                break;
            }
            case SemanticSynchrony.SHORTCUT:
                if (value.length() > 0) {
                    note.setShortcut(value);
                } else {
                    note.setShortcut(Note.CLEARME_VALUE);
                }
                break;
            case SemanticSynchrony.WEIGHT: {
                note.setWeight(getFloat(key, value, lineNumber));
                break;
            }
            default:
                throw new NoteParsingException(lineNumber, "unknown property: " + key);
        }
    }

    public Note fromJSON(final JSONObject json) throws JSONException {
        Note n = new Note();

        if (json.has(NoteWriter.ID)) {
            n.setId(json.getString(NoteWriter.ID));
        }
        if (json.has(SemanticSynchrony.VALUE)) {
            n.setValue(json.getString(SemanticSynchrony.VALUE));
        }
        if (json.has(SemanticSynchrony.ALIAS)) {
            n.setAlias(json.getString(SemanticSynchrony.ALIAS));
        }
        if (json.has(SemanticSynchrony.SHORTCUT)) {
            n.setShortcut(json.getString(SemanticSynchrony.SHORTCUT));
        }
        if (json.has(SemanticSynchrony.SHARABILITY)) {
            n.setSharability((float) json.getDouble(SemanticSynchrony.SHARABILITY));
        }
        if (json.has(SemanticSynchrony.WEIGHT)) {
            n.setWeight((float) json.getDouble(SemanticSynchrony.WEIGHT));
        }
        if (json.has(SemanticSynchrony.PRIORITY)) {
            n.setPriority((float) json.getDouble(SemanticSynchrony.PRIORITY));
        }
        if (json.has(SemanticSynchrony.CREATED)) {
            n.setCreated(json.getLong(SemanticSynchrony.CREATED));
        }
        if (json.has(NoteWriter.HAS_CHILDREN)) {
            n.setHasChildren(json.optBoolean(NoteWriter.HAS_CHILDREN));
        }

        JSONArray a = json.optJSONArray(NoteWriter.CHILDREN);
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
