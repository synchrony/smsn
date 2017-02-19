package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.BrainParser;
import net.fortytwo.smsn.brain.model.Note;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;

public class JsonParser extends BrainParser {

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
        if (json.has(SemanticSynchrony.PropertyKeys.TITLE)) {
            n.setTitle(json.getString(SemanticSynchrony.PropertyKeys.TITLE));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.ALIAS)) {
            n.setAlias(json.getString(SemanticSynchrony.PropertyKeys.ALIAS));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.SHORTCUT)) {
            n.setShortcut(json.getString(SemanticSynchrony.PropertyKeys.SHORTCUT));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.SHARABILITY)) {
            n.setSharability((float) json.getDouble(SemanticSynchrony.PropertyKeys.SHARABILITY));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.WEIGHT)) {
            n.setWeight((float) json.getDouble(SemanticSynchrony.PropertyKeys.WEIGHT));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.PRIORITY)) {
            n.setPriority((float) json.getDouble(SemanticSynchrony.PropertyKeys.PRIORITY));
        }
        if (json.has(SemanticSynchrony.PropertyKeys.CREATED)) {
            n.setCreated(json.getLong(SemanticSynchrony.PropertyKeys.CREATED));
        }
        if (json.has(JsonFormat.NUMBER_OF_CHILDREN)) {
            n.setNumberOfChildren(json.optInt(JsonFormat.NUMBER_OF_CHILDREN));
        }
        if (json.has(JsonFormat.NUMBER_OF_PARENTS)) {
            n.setNumberOfParents(json.optInt(JsonFormat.NUMBER_OF_PARENTS));
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
