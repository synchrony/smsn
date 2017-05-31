package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Note;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

public class JsonPrinter {

    private int titleLengthCutoff = -1;

    public int getTitleLengthCutoff() {
        return titleLengthCutoff;
    }

    public void setTitleLengthCutoff(int titleLengthCutoff) {
        this.titleLengthCutoff = titleLengthCutoff;
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

        json.put(JsonFormat.Keys.ID, note.getId());
        json.put(SemanticSynchrony.PropertyKeys.WEIGHT, note.getWeight());
        json.put(SemanticSynchrony.PropertyKeys.SOURCE, note.getSource());
        json.put(SemanticSynchrony.PropertyKeys.CREATED, note.getCreated());
        json.put(JsonFormat.Keys.NUMBER_OF_CHILDREN, note.getNumberOfChildren());
        json.put(JsonFormat.Keys.NUMBER_OF_PARENTS, note.getNumberOfParents());

        Float priority = note.getPriority();
        if (null != priority && priority > 0) {
            json.put(SemanticSynchrony.PropertyKeys.PRIORITY, priority);
        }

        String title = note.getTitle();
        if (title != null && titleLengthCutoff > 0 && title.length() > titleLengthCutoff) {
            title = title.substring(0, titleLengthCutoff) + JsonFormat.TITLE_TRUNCATOR;
        }
        json.put(SemanticSynchrony.PropertyKeys.TITLE, title);

        if (null != note.getText()) {
            json.put(SemanticSynchrony.PropertyKeys.PAGE, note.getText());
        }

        if (null != note.getAlias()) {
            json.put(SemanticSynchrony.PropertyKeys.ALIAS, note.getAlias());
        }

        if (null != note.getShortcut()) {
            json.put(SemanticSynchrony.PropertyKeys.SHORTCUT, note.getShortcut());
        }

        if (null != note.getMeta()) {
            JSONArray c = new JSONArray();
            json.put(JsonFormat.Keys.META, c);
            int i = 0;
            for (String s : note.getMeta()) {
                c.put(i++, s);
            }
        }

        if (0 < note.getChildren().size()) {
            JSONArray c = new JSONArray();
            json.put(JsonFormat.Keys.CHILDREN, c);
            int i = 0;
            for (Note child : note.getChildren()) {
                c.put(i, toJsonInternal(child));
                i++;
            }
        }

        return json;
    }
}
