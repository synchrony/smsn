package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.NoteParser;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.query.Model;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;

public class JsonParser extends NoteParser {

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
        NoteDTO note = new NoteDTO();

        if (json.has(JsonFormat.Keys.ID)) {
            Model.setTopicId(note, json.getString(JsonFormat.Keys.ID));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.LABEL)) {
            note.setLabel(json.getString(SemanticSynchrony.PropertyKeys.LABEL));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.ALIAS)) {
            note.setAlias(json.getString(SemanticSynchrony.PropertyKeys.ALIAS));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.SHORTCUT)) {
            note.setShortcut(json.getString(SemanticSynchrony.PropertyKeys.SHORTCUT));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.SOURCE)) {
            note.setSource(json.getString(SemanticSynchrony.PropertyKeys.SOURCE));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.WEIGHT)) {
            note.setWeight((float) json.getDouble(SemanticSynchrony.PropertyKeys.WEIGHT));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.PRIORITY)) {
            note.setPriority((float) json.getDouble(SemanticSynchrony.PropertyKeys.PRIORITY));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.CREATED)) {
            note.setCreated(json.getLong(SemanticSynchrony.PropertyKeys.CREATED));
        }
        if (json.has(JsonFormat.Keys.NUMBER_OF_CHILDREN)) {
            note.setNumberOfChildren(json.optInt(JsonFormat.Keys.NUMBER_OF_CHILDREN));
        }
        if (json.has(JsonFormat.Keys.NUMBER_OF_PARENTS)) {
            note.setNumberOfParents(json.optInt(JsonFormat.Keys.NUMBER_OF_PARENTS));
        }

        JSONArray a = json.optJSONArray(JsonFormat.Keys.CHILDREN);
        if (null != a) {
            Note[] children = new Note[a.length()];
            for (int i = 0; i < a.length(); i++) {
                JSONObject jc = a.getJSONObject(i);

                children[i] = parse(jc);
            }
            Note.setChildren(note, children);
        }

        return note;
    }
}
