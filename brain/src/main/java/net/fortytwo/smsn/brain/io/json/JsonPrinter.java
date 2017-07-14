package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.util.List;

public class JsonPrinter {

    private int titleLengthCutoff = -1;

    public int getTitleLengthCutoff() {
        return titleLengthCutoff;
    }

    public void setTitleLengthCutoff(int titleLengthCutoff) {
        this.titleLengthCutoff = titleLengthCutoff;
    }

    public JSONObject toJson(final TreeNode<Link> node) throws IOException {
        try {
            return toJsonInternal(node);
        } catch (JSONException e) {
            throw new IOException(e);
        }
    }

    private void putPriority(final TreeNode<Link> node, final JSONObject json) throws JSONException {
        Float priority = TreeViews.getPriority(node);
        if (null != priority && priority > 0) {
            json.put(SemanticSynchrony.PropertyKeys.PRIORITY, priority);
        }
    }

    private void putTitle(final TreeNode<Link> node, final JSONObject json) throws JSONException {
        String title = TreeViews.getTitle(node);
        if (title != null && titleLengthCutoff > 0 && title.length() > titleLengthCutoff) {
            title = title.substring(0, titleLengthCutoff) + JsonFormat.TITLE_TRUNCATOR;
        }
        json.put(SemanticSynchrony.PropertyKeys.TITLE, title);
    }

    private void putText(final TreeNode<Link> node, final JSONObject json) throws JSONException {
        String text = TreeViews.getText(node);
        if (null != text) {
            json.put(SemanticSynchrony.PropertyKeys.TEXT, text);
        }
    }

    private void putAlias(final TreeNode<Link> node, final JSONObject json) throws JSONException {
        String alias = TreeViews.getAlias(node);
        if (null != alias) {
            json.put(SemanticSynchrony.PropertyKeys.ALIAS, alias);
        }
    }

    private void putShortcut(final TreeNode<Link> node, final JSONObject json) throws JSONException {
        String shortcut = TreeViews.getShortcut(node);
        if (null != shortcut) {
            json.put(SemanticSynchrony.PropertyKeys.SHORTCUT, shortcut);
        }
    }

    private void putMeta(final TreeNode<Link> node, final JSONObject json) throws JSONException {
        List<String> meta = TreeViews.getMeta(node);
        if (null != meta && 0 != meta.size()) {
            JSONArray c = new JSONArray();
            json.put(JsonFormat.Keys.META, c);
            int i = 0;
            for (String s : meta) {
                c.put(i++, s);
            }
        }
    }

    private void putChildren(final TreeNode<Link> node, final JSONObject json) throws JSONException {
        if (null != node.getChildren()) {
            JSONArray c = new JSONArray();
            json.put(JsonFormat.Keys.CHILDREN, c);
            int i = 0;
            // TODO: put
            for (TreeNode<Link> child : ListNode.toJavaList(node.getChildren())) {
                c.put(i, toJsonInternal(child));
                i++;
            }
        }
    }

    public JSONObject toJsonInternal(final TreeNode<Link> node) throws JSONException {
        JSONObject json = new JSONObject();

        json.put(JsonFormat.Keys.ID, TreeViews.getId(node));
        json.put(SemanticSynchrony.PropertyKeys.WEIGHT, TreeViews.getWeight(node));
        json.put(SemanticSynchrony.PropertyKeys.SOURCE, TreeViews.getSource(node));
        json.put(SemanticSynchrony.PropertyKeys.CREATED, TreeViews.getCreated(node));
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
