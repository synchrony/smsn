package net.fortytwo.smsn.brain.io.json;

import hydra.util.Opt;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Normed;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.Timestamp;
import net.fortytwo.smsn.brain.TreeNode;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 * Parser for converting JSON to immutable TreeNode objects.
 * This replaces the old JsonParser which worked with mutable TreeNode<Link>.
 */
public class TreeNodeJsonParser {

    public TreeNode parse(final String jsonString) throws IOException {
        try {
            JSONObject json = new JSONObject(jsonString);
            return parseTreeNode(json);
        } catch (JSONException e) {
            throw new IOException("Failed to parse JSON", e);
        }
    }

    public TreeNode parse(final InputStream in) throws IOException {
        try {
            String jsonString = new String(in.readAllBytes(), StandardCharsets.UTF_8);
            return parse(jsonString);
        } catch (IOException e) {
            throw new IOException("Failed to read JSON stream", e);
        }
    }

    private TreeNode parseTreeNode(final JSONObject json) throws JSONException {
        // Required fields
        AtomId id = json.has(JsonFormat.Keys.ID) && !json.isNull(JsonFormat.Keys.ID)
                ? new AtomId(json.getString(JsonFormat.Keys.ID))
                : new AtomId("temp-" + System.nanoTime());

        Timestamp created = json.has(SemanticSynchrony.PropertyKeys.CREATED)
                ? new Timestamp((int) json.getLong(SemanticSynchrony.PropertyKeys.CREATED))
                : new Timestamp((int) (System.currentTimeMillis() / 1000));

        Normed weight = json.has(SemanticSynchrony.PropertyKeys.WEIGHT)
                ? new Normed((float) json.getDouble(SemanticSynchrony.PropertyKeys.WEIGHT))
                : new Normed(0.5f);

        SourceName source = json.has(SemanticSynchrony.PropertyKeys.SOURCE)
                ? new SourceName(json.getString(SemanticSynchrony.PropertyKeys.SOURCE))
                : new SourceName("public");

        String title = json.optString(SemanticSynchrony.PropertyKeys.TITLE, "");

        // Optional fields
        Opt<Normed> priority = json.has(SemanticSynchrony.PropertyKeys.PRIORITY)
                ? Opt.of(new Normed((float) json.getDouble(SemanticSynchrony.PropertyKeys.PRIORITY)))
                : Opt.empty();

        Opt<String> alias = json.has(SemanticSynchrony.PropertyKeys.ALIAS)
                ? Opt.of(json.getString(SemanticSynchrony.PropertyKeys.ALIAS))
                : Opt.empty();

        Opt<String> text = json.has(SemanticSynchrony.PropertyKeys.TEXT)
                ? Opt.of(json.getString(SemanticSynchrony.PropertyKeys.TEXT))
                : Opt.empty();

        Opt<String> shortcut = json.has(SemanticSynchrony.PropertyKeys.SHORTCUT)
                ? Opt.of(json.getString(SemanticSynchrony.PropertyKeys.SHORTCUT))
                : Opt.empty();

        // Parse children recursively
        List<TreeNode> children = new ArrayList<>();
        if (json.has(JsonFormat.Keys.CHILDREN)) {
            JSONArray childrenArray = json.getJSONArray(JsonFormat.Keys.CHILDREN);
            for (int i = 0; i < childrenArray.length(); i++) {
                JSONObject childJson = childrenArray.getJSONObject(i);
                children.add(parseTreeNode(childJson));
            }
        }

        // Counts
        int numberOfChildren = json.optInt(JsonFormat.Keys.NUMBER_OF_CHILDREN, children.size());
        int numberOfParents = json.optInt(JsonFormat.Keys.NUMBER_OF_PARENTS, 0);

        return new TreeNode(
                id, created, weight, priority, source, title, alias, text, shortcut,
                children, numberOfChildren, numberOfParents
        );
    }
}
