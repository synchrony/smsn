package net.fortytwo.smsn.brain.io.json;

import hydra.util.Opt;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Normed;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.Timestamp;
import net.fortytwo.smsn.brain.TreeNode;
import net.fortytwo.smsn.brain.view.TreeUpdate;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Parser for converting JSON to immutable TreeNode objects.
 * This replaces the old JsonParser which worked with mutable TreeNode<Link>.
 *
 * <p>Like {@link net.fortytwo.smsn.brain.io.wiki.TreeNodeWikiParser},
 * this parser provides two entry points:
 *
 * <ul>
 *   <li>{@link #parse(String)} / {@link #parse(InputStream)} return a
 *       plain {@link TreeNode}, with defaults filled in for missing
 *       properties.
 *   <li>{@link #parseUpdate(String)} / {@link #parseUpdate(InputStream)}
 *       return a {@link TreeUpdate} carrying side-channel information
 *       about which properties were explicitly present in the JSON.
 * </ul>
 *
 * For JSON the side-channel is straightforward: a property is "touched"
 * iff its key appears in the JSON object. Children are considered
 * "addressed" iff the {@code children} key is present at all (so an
 * empty array means "user wants no children", while a missing
 * {@code children} key means "user is not addressing children" — same
 * semantics as the wiki format's collapsed-bullet marker).
 */
public class TreeNodeJsonParser {

    public TreeNode parse(final String jsonString) throws IOException {
        return parseUpdate(jsonString).getTree();
    }

    public TreeNode parse(final InputStream in) throws IOException {
        return parseUpdate(in).getTree();
    }

    public TreeUpdate parseUpdate(final String jsonString) throws IOException {
        try {
            JSONObject json = new JSONObject(jsonString);
            Map<AtomId, Set<String>> touched = new HashMap<>();
            Set<AtomId> collapsed = new HashSet<>();
            Set<String> rootTouched = touchedKeysFor(json);
            TreeNode tree = parseTreeNode(json, touched, collapsed);
            return new TreeUpdate(tree, rootTouched, touched, collapsed);
        } catch (JSONException e) {
            throw new IOException("Failed to parse JSON", e);
        }
    }

    public TreeUpdate parseUpdate(final InputStream in) throws IOException {
        try {
            String jsonString = new String(in.readAllBytes(), StandardCharsets.UTF_8);
            return parseUpdate(jsonString);
        } catch (IOException e) {
            throw new IOException("Failed to read JSON stream", e);
        }
    }

    /** Map a JSONObject's keys to the side-channel "touched" property
     *  names used by {@link TreeUpdate}. */
    private Set<String> touchedKeysFor(final JSONObject json) {
        Set<String> touched = new HashSet<>();
        for (String key : new String[] {
                SemanticSynchrony.PropertyKeys.TITLE,
                SemanticSynchrony.PropertyKeys.WEIGHT,
                SemanticSynchrony.PropertyKeys.ALIAS,
                SemanticSynchrony.PropertyKeys.TEXT,
                SemanticSynchrony.PropertyKeys.SHORTCUT,
                SemanticSynchrony.PropertyKeys.PRIORITY,
        }) {
            if (json.has(key)) {
                touched.add(key);
            }
        }
        return touched;
    }

    private TreeNode parseTreeNode(
            final JSONObject json,
            final Map<AtomId, Set<String>> touched,
            final Set<AtomId> collapsed) throws JSONException {
        // Required fields
        AtomId id = json.has(JsonFormat.Keys.ID) && !json.isNull(JsonFormat.Keys.ID)
                ? new AtomId(json.getString(JsonFormat.Keys.ID))
                : new AtomId("temp-" + System.nanoTime());

        Timestamp created = json.has(SemanticSynchrony.PropertyKeys.CREATED)
                ? new Timestamp(json.getLong(SemanticSynchrony.PropertyKeys.CREATED))
                : new Timestamp(System.currentTimeMillis());

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

        // Record per-atom side-channel for this node (skip the "temp-..." id).
        if (id != null && !id.value.startsWith("temp-")) {
            Set<String> nodeTouched = touchedKeysFor(json);
            if (!nodeTouched.isEmpty()) {
                touched.computeIfAbsent(id, k -> new HashSet<>()).addAll(nodeTouched);
            }
            // If the JSON does not include a "children" key at all, treat
            // this node as collapsed (the user is not addressing its
            // children). An empty array still counts as "addressed".
            if (!json.has(JsonFormat.Keys.CHILDREN)) {
                collapsed.add(id);
            }
        }

        // Parse children recursively
        List<TreeNode> children = new ArrayList<>();
        if (json.has(JsonFormat.Keys.CHILDREN)) {
            JSONArray childrenArray = json.getJSONArray(JsonFormat.Keys.CHILDREN);
            for (int i = 0; i < childrenArray.length(); i++) {
                JSONObject childJson = childrenArray.getJSONObject(i);
                children.add(parseTreeNode(childJson, touched, collapsed));
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
