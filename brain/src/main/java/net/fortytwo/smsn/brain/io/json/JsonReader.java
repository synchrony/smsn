package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.NoteReader;
import net.fortytwo.smsn.brain.model.Note;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;

public class JsonReader extends NoteReader {

    @Override
    public Note parse(final InputStream in) throws IOException {
        JSONObject json = new JSONObject(in);
        try {
            return parse(json);
        } catch (JSONException e) {
            throw new IOException(e);
        }
    }

    private Note parse(final JSONObject json) throws JSONException {
        Note n = new Note();

        if (json.has(JsonFormat.ID)) {
            n.setId(json.getString(JsonFormat.ID));
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
        if (json.has(JsonFormat.HAS_CHILDREN)) {
            n.setHasChildren(json.optBoolean(JsonFormat.HAS_CHILDREN));
        }

        JSONArray a = json.optJSONArray(JsonFormat.CHILDREN);
        if (null != a) {
            for (int i = 0; i < a.length(); i++) {
                JSONObject jc = a.getJSONObject(i);
                n.addChild(parse(jc));
            }
        }

        return n;
    }
}
