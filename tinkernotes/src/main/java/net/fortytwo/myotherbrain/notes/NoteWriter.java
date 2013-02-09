package net.fortytwo.myotherbrain.notes;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteWriter {

    public static final String VALUE_TRUNCATOR = " [...]";

    public static final String
            ALIAS = "alias",
            CHILDREN = "children",
            CREATED = "created",
            HAS_CHILDREN = "hasChildren",
            ID = "id",
            SHARABILITY = "sharability",
            VALUE = "value",
            WEIGHT = "weight";

    private int valueLengthCutoff = -1;

    public JSONObject toJSON(final Note n) throws JSONException {
        JSONObject json = new JSONObject();

        json.put(ID, n.getId());
        json.put(WEIGHT, n.getWeight());
        json.put(SHARABILITY, n.getSharability());
        json.put(CREATED, n.getCreated());
        json.put(HAS_CHILDREN, n.getHasChildren());

        String value = n.getValue();
        if (value != null && valueLengthCutoff > 0 && value.length() > valueLengthCutoff) {
            value = value.substring(0, valueLengthCutoff) + VALUE_TRUNCATOR;
        }
        json.put(VALUE, value);

        if (null != n.getAlias()) {
            json.put(ALIAS, n.getAlias());
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
            if (Character.isISOControl(c)) {
                return false;
            }
        }

        return true;
    }

    public int getValueLengthCutoff() {
        return valueLengthCutoff;
    }

    public void setValueLengthCutoff(int valueLengthCutoff) {
        this.valueLengthCutoff = valueLengthCutoff;
    }
}
