package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.TreeNode;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

/**
 * Printer for converting immutable TreeNode objects to JSON.
 * This replaces the old JsonPrinter which worked with mutable TreeNode<Link>.
 */
public class TreeNodeJsonPrinter {

    private int titleLengthCutoff = -1;

    public int getTitleLengthCutoff() {
        return titleLengthCutoff;
    }

    public void setTitleLengthCutoff(int titleLengthCutoff) {
        this.titleLengthCutoff = titleLengthCutoff;
    }

    public JSONObject toJson(final TreeNode node) throws IOException {
        try {
            return toJsonInternal(node);
        } catch (JSONException e) {
            throw new IOException("Failed to serialize TreeNode to JSON", e);
        }
    }

    public String toJsonString(final TreeNode node) throws IOException {
        return toJson(node).toString(2); // Pretty print with 2-space indent
    }

    private JSONObject toJsonInternal(final TreeNode node) throws JSONException {
        JSONObject json = new JSONObject();

        // Required fields
        json.put(JsonFormat.Keys.ID, node.id.value);
        json.put(SemanticSynchrony.PropertyKeys.CREATED, node.created.value);
        json.put(SemanticSynchrony.PropertyKeys.WEIGHT, node.weight.value);
        json.put(SemanticSynchrony.PropertyKeys.SOURCE, node.source.value);

        // Title with optional truncation
        String title = node.title;
        if (title != null && titleLengthCutoff > 0 && title.length() > titleLengthCutoff) {
            title = title.substring(0, titleLengthCutoff) + JsonFormat.TITLE_TRUNCATOR;
        }
        json.put(SemanticSynchrony.PropertyKeys.TITLE, title);

        // Counts
        json.put(JsonFormat.Keys.NUMBER_OF_CHILDREN, node.numberOfChildren);
        json.put(JsonFormat.Keys.NUMBER_OF_PARENTS, node.numberOfParents);

        // Optional fields
        if (node.priority.isPresent()) {
            json.put(SemanticSynchrony.PropertyKeys.PRIORITY, node.priority.get().value);
        }

        if (node.alias.isPresent()) {
            json.put(SemanticSynchrony.PropertyKeys.ALIAS, node.alias.get());
        }

        if (node.text.isPresent()) {
            json.put(SemanticSynchrony.PropertyKeys.TEXT, node.text.get());
        }

        if (node.shortcut.isPresent()) {
            json.put(SemanticSynchrony.PropertyKeys.SHORTCUT, node.shortcut.get());
        }

        // Children
        if (node.children != null && !node.children.isEmpty()) {
            JSONArray childrenArray = new JSONArray();
            for (TreeNode child : node.children) {
                childrenArray.put(toJsonInternal(child));
            }
            json.put(JsonFormat.Keys.CHILDREN, childrenArray);
        }

        return json;
    }
}
