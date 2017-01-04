package net.fortytwo.smsn.brain.wiki;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Note;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.util.List;

public class NoteWriter {

    public static final String VALUE_TRUNCATOR = " [...]";

    public static final String CHILDREN = "children";
    public static final String HAS_CHILDREN = "hasChildren";
    public static final String ID = "id";
    public static final String META = "meta";

    private int valueLengthCutoff = -1;

    public int getValueLengthCutoff() {
        return valueLengthCutoff;
    }

    public void setValueLengthCutoff(int valueLengthCutoff) {
        this.valueLengthCutoff = valueLengthCutoff;
    }

    public JSONObject toJSON(final Note n) throws JSONException {
        JSONObject json = new JSONObject();

        json.put(ID, n.getId());
        json.put(SemanticSynchrony.WEIGHT, n.getWeight());
        json.put(SemanticSynchrony.SHARABILITY, n.getSharability());
        json.put(SemanticSynchrony.CREATED, n.getCreated());
        json.put(HAS_CHILDREN, n.getHasChildren());

        Float priority = n.getPriority();
        if (null != priority && priority > 0) {
            json.put(SemanticSynchrony.PRIORITY, priority);
        }

        String value = n.getValue();
        if (value != null && valueLengthCutoff > 0 && value.length() > valueLengthCutoff) {
            value = value.substring(0, valueLengthCutoff) + VALUE_TRUNCATOR;
        }
        json.put(SemanticSynchrony.VALUE, value);

        if (null != n.getAlias()) {
            json.put(SemanticSynchrony.ALIAS, n.getAlias());
        }

        if (null != n.getShortcut()) {
            json.put(SemanticSynchrony.SHORTCUT, n.getShortcut());
        }

        if (null != n.getMeta()) {
            JSONArray c = new JSONArray();
            json.put(META, c);
            int i = 0;
            for (String s : n.getMeta()) {
                c.put(i++, s);
            }
        }

        if (0 < n.getChildren().size()) {
            JSONArray c = new JSONArray();
            json.put(CHILDREN, c);
            int i = 0;
            for (Note child : n.getChildren()) {
                c.put(i, toJSON(child));
                i++;
            }
        }

        return json;
    }

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
            p.print(escapeValue(sanitizeValue(n.getValue())));
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
            return NoteReader.VERBATIM_BLOCK_START + "\n" + value + "\n" + NoteReader.VERBATIM_BLOCK_END;
        } else {
            return value;
        }
    }

    private static String sanitizeValue(final String value) {
        return null == value ? ""
                : !isValidValue(value)
                ? "???"
                : value;
    }

    private static String padKey(String id) {
        while (id.length() < 5) {
            id = "0" + id;
        }

        return id;
    }

    private static boolean isValidValue(final String value) {
        for (char c : value.toCharArray()) {
            // newline is the only control character allowed
            if (c != '\n' && c != '\r' && Character.isISOControl(c)) {
                return false;
            }
        }

        return true;
    }
}
