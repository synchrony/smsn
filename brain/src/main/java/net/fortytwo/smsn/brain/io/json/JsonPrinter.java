package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.query.Model;
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

    public JSONObject toJson(final Note node) throws IOException {
        try {
            return toJsonInternal(node);
        } catch (JSONException e) {
            throw new IOException(e);
        }
    }

    private void putPriority(final Note node, final JSONObject json) throws JSONException {
        Float priority = node.getPriority();
        if (null != priority && priority > 0) {
            json.put(SemanticSynchrony.PropertyKeys.PRIORITY, priority);
        }
    }

    private void putTitle(final Note node, final JSONObject json) throws JSONException {
        String title = node.getLabel();
        if (title != null && titleLengthCutoff > 0 && title.length() > titleLengthCutoff) {
            title = title.substring(0, titleLengthCutoff) + JsonFormat.TITLE_TRUNCATOR;
        }
        json.put(SemanticSynchrony.PropertyKeys.LABEL, title);
    }

    private void putText(final Note node, final JSONObject json) throws JSONException {
        String text = node.getText();
        if (null != text) {
            json.put(SemanticSynchrony.PropertyKeys.TEXT, text);
        }
    }

    private void putAlias(final Note node, final JSONObject json) throws JSONException {
        String alias = node.getAlias();
        if (null != alias) {
            json.put(SemanticSynchrony.PropertyKeys.ALIAS, alias);
        }
    }

    private void putShortcut(final Note node, final JSONObject json) throws JSONException {
        String shortcut = node.getShortcut();
        if (null != shortcut) {
            json.put(SemanticSynchrony.PropertyKeys.SHORTCUT, shortcut);
        }
    }

    private void putMeta(final Note node, final JSONObject json) throws JSONException {
        /*
        List<String> meta = Note.getMeta(node);
        if (null != meta && 0 != meta.size()) {
            JSONArray c = new JSONArray();
            json.put(JsonFormat.Keys.META, c);
            int i = 0;
            for (String s : meta) {
                c.put(i++, s);
            }
        }
        */
    }

    private void putChildren(final Note node, final JSONObject json) throws JSONException {
        if (null != node.getFirst()) {
            JSONArray c = new JSONArray();
            json.put(JsonFormat.Keys.CHILDREN, c);
            int i = 0;
            // TODO: put
            for (Note child : ListNode.toJavaList(node.getFirst())) {
                c.put(i, toJsonInternal(child));
                i++;
            }
        }
    }

    public JSONObject toJsonInternal(final Note node) throws JSONException {
        JSONObject json = new JSONObject();

        json.put(JsonFormat.Keys.ID, Model.getTopicId(node));
        json.put(SemanticSynchrony.PropertyKeys.WEIGHT, node.getWeight());
        json.put(SemanticSynchrony.PropertyKeys.SOURCE, node.getSource());
        json.put(SemanticSynchrony.PropertyKeys.CREATED, node.getCreated());
        json.put(JsonFormat.Keys.NUMBER_OF_CHILDREN, node.getNumberOfChildren());
        json.put(JsonFormat.Keys.NUMBER_OF_PARENTS, node.getNumberOfParents());

        putPriority(node, json);
        putTitle(node, json);
        putText(node, json);
        putAlias(node, json);
        putShortcut(node, json);
            putMeta(node, json);
            putChildren(node, json);

        return json;
    }
}
