package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Note;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

public class JsonPrinter {

    private int valueLengthCutoff = -1;

    public int getValueLengthCutoff() {
        return valueLengthCutoff;
    }

    public void setValueLengthCutoff(int valueLengthCutoff) {
        this.valueLengthCutoff = valueLengthCutoff;
    }

    public JSONObject toJson(final Note n) throws IOException {
        try {
            return toJsonInternal(n);
        } catch (JSONException e) {
            throw new IOException(e);
        }
    }

    public JSONObject toJsonInternal(final Note note) throws JSONException {
        JSONObject json = new JSONObject();

        json.put(JsonFormat.ID, note.getId());
        json.put(SemanticSynchrony.WEIGHT, note.getWeight());
        json.put(SemanticSynchrony.SHARABILITY, note.getSharability());
        json.put(SemanticSynchrony.CREATED, note.getCreated());
        json.put(JsonFormat.NUMBER_OF_CHILDREN, note.getNumberOfChildren());
        json.put(JsonFormat.NUMBER_OF_PARENTS, note.getNumberOfParents());

        Float priority = note.getPriority();
        if (null != priority && priority > 0) {
            json.put(SemanticSynchrony.PRIORITY, priority);
        }

        String value = note.getTitle();
        if (value != null && valueLengthCutoff > 0 && value.length() > valueLengthCutoff) {
            value = value.substring(0, valueLengthCutoff) + JsonFormat.VALUE_TRUNCATOR;
        }
        json.put(SemanticSynchrony.TITLE, value);

        if (null != note.getAlias()) {
            json.put(SemanticSynchrony.ALIAS, note.getAlias());
        }

        if (null != note.getShortcut()) {
            json.put(SemanticSynchrony.SHORTCUT, note.getShortcut());
        }

        if (null != note.getMeta()) {
            JSONArray c = new JSONArray();
            json.put(JsonFormat.META, c);
            int i = 0;
            for (String s : note.getMeta()) {
                c.put(i++, s);
            }
        }

        if (0 < note.getChildren().size()) {
            JSONArray c = new JSONArray();
            json.put(JsonFormat.CHILDREN, c);
            int i = 0;
            for (Note child : note.getChildren()) {
                c.put(i, toJsonInternal(child));
                i++;
            }
        }

        return json;
    }
}
