package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Note;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

public class JsonWriter {

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

    public JSONObject toJsonInternal(final Note n) throws JSONException {
        JSONObject json = new JSONObject();

        json.put(JsonFormat.ID, n.getId());
        json.put(SemanticSynchrony.WEIGHT, n.getWeight());
        json.put(SemanticSynchrony.SHARABILITY, n.getSharability());
        json.put(SemanticSynchrony.CREATED, n.getCreated());
        json.put(JsonFormat.HAS_CHILDREN, n.getHasChildren());

        Float priority = n.getPriority();
        if (null != priority && priority > 0) {
            json.put(SemanticSynchrony.PRIORITY, priority);
        }

        String value = n.getValue();
        if (value != null && valueLengthCutoff > 0 && value.length() > valueLengthCutoff) {
            value = value.substring(0, valueLengthCutoff) + JsonFormat.VALUE_TRUNCATOR;
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
            json.put(JsonFormat.META, c);
            int i = 0;
            for (String s : n.getMeta()) {
                c.put(i++, s);
            }
        }

        if (0 < n.getChildren().size()) {
            JSONArray c = new JSONArray();
            json.put(JsonFormat.CHILDREN, c);
            int i = 0;
            for (Note child : n.getChildren()) {
                c.put(i, toJsonInternal(child));
                i++;
            }
        }

        return json;
    }
}
