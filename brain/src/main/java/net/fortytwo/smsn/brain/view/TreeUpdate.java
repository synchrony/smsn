package net.fortytwo.smsn.brain.view;

import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.TreeNode;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * A user-edit submission expressed as a {@link TreeNode} plus side-channel
 * metadata describing the user's intent.
 *
 * <p>This is the input to {@link TreeUpdater#update(TreeUpdate, int,
 * net.fortytwo.smsn.brain.model.Filter)}. It is produced by parsers
 * (e.g. {@code TreeNodeWikiParser}, {@code TreeNodeJsonParser}) when
 * converting a user-submitted view into something the updater can act on.
 *
 * <h3>Why this exists</h3>
 *
 * The {@link TreeNode} record always carries values for every required
 * property (e.g. {@code weight}, {@code children}). When a parser builds
 * a {@code TreeNode} from a user-submitted wiki view, it has to fill in
 * defaults for any property the user didn't write — leaving the updater
 * unable to distinguish "the user meant to set this to the default" from
 * "the user said nothing about this property".
 *
 * <p>Without that distinction, the updater silently clobbers every
 * stored property the user didn't address, on every save. (See
 * <code>kg/SMSN_BUG_REPORT.md</code> in the data repo.)
 *
 * <p>This class threads the missing information from parsers to the
 * updater without changing any Hydra-generated schema. The parsers
 * record, for each atom-id appearing in the submission, two pieces of
 * intent:
 *
 * <ul>
 *   <li>{@link #getTouchedProperties} — which {@code @key} lines the
 *       user explicitly wrote in the view for that atom.
 *   <li>{@link #getCollapsedAtoms} — atom-ids the user rendered with
 *       a collapsed bullet ({@code · :id:}), meaning "I am not
 *       addressing this atom's children".
 * </ul>
 *
 * The updater consults these sets before issuing any
 * {@code repository.updateProperty(...)} or {@code deleteChildAt(...)}
 * call, so omissions in the view never overwrite stored data.
 */
public final class TreeUpdate {

    private final TreeNode tree;
    private final Set<String> rootTouched;
    private final Map<AtomId, Set<String>> touchedProperties;
    private final Set<AtomId> collapsedAtoms;

    public TreeUpdate(
            final TreeNode tree,
            final Set<String> rootTouched,
            final Map<AtomId, Set<String>> touchedProperties,
            final Set<AtomId> collapsedAtoms) {
        if (tree == null) {
            throw new IllegalArgumentException("tree must not be null");
        }
        this.tree = tree;
        this.rootTouched = rootTouched == null
                ? Collections.emptySet()
                : rootTouched;
        this.touchedProperties = touchedProperties == null
                ? Collections.emptyMap()
                : touchedProperties;
        this.collapsedAtoms = collapsedAtoms == null
                ? Collections.emptySet()
                : collapsedAtoms;
    }

    /** A {@code TreeUpdate} that asserts every property of every node
     *  in the tree (no side-channel). Use only when the source genuinely
     *  is asserting all properties — never use this for user-submitted
     *  edits. Provided for tests and for legacy callers that haven't been
     *  migrated yet. */
    public static TreeUpdate ofAllAsserted(final TreeNode tree) {
        Set<String> rootTouched = touchedFor(tree);
        Map<AtomId, Set<String>> touched = new HashMap<>();
        markAllAsserted(tree, touched);
        return new TreeUpdate(tree, rootTouched, touched, Collections.emptySet());
    }

    private static Set<String> touchedFor(final TreeNode node) {
        Set<String> all = new HashSet<>();
        all.add("title");
        all.add("weight");
        if (node.alias.isPresent())    all.add("alias");
        if (node.text.isPresent())     all.add("text");
        if (node.shortcut.isPresent()) all.add("shortcut");
        if (node.priority.isPresent()) all.add("priority");
        return all;
    }

    private static void markAllAsserted(
            final TreeNode node,
            final Map<AtomId, Set<String>> touched) {
        if (node.id != null) {
            touched.put(node.id, touchedFor(node));
        }
        for (TreeNode child : node.children) {
            markAllAsserted(child, touched);
        }
    }

    public TreeNode getTree() {
        return tree;
    }

    /** Property names the user wrote at the top of the submitted view —
     *  i.e., applied to the root atom of the edit. The wiki parser
     *  collects these from {@code @<key>} lines that appear before any
     *  bullet, since the root atom's id is identified out-of-band by
     *  the request rather than appearing in the wiki text itself. */
    public Set<String> getRootTouchedProperties() {
        return Collections.unmodifiableSet(rootTouched);
    }

    public boolean isRootPropertyTouched(final String propertyKey) {
        return rootTouched.contains(propertyKey);
    }

    /** For atom-id {@code id}, the set of property names the user
     *  explicitly wrote in their submission. Property names match the
     *  {@code @<key>} line keys: "title", "weight", "alias", "text",
     *  "shortcut", "priority". Empty / missing set means the user wrote
     *  no {@code @<key>} lines for that atom. */
    public Set<String> propertiesTouchedFor(final AtomId id) {
        Set<String> s = touchedProperties.get(id);
        return s == null ? Collections.emptySet() : s;
    }

    public boolean isPropertyTouched(final AtomId id, final String propertyKey) {
        Set<String> s = touchedProperties.get(id);
        return s != null && s.contains(propertyKey);
    }

    /** True iff the user rendered this atom with a collapsed bullet
     *  ({@code · :id:}), indicating the children of this atom were
     *  not addressed by the submission. */
    public boolean isCollapsed(final AtomId id) {
        return collapsedAtoms.contains(id);
    }

    public Map<AtomId, Set<String>> getTouchedProperties() {
        return Collections.unmodifiableMap(touchedProperties);
    }

    public Set<AtomId> getCollapsedAtoms() {
        return Collections.unmodifiableSet(collapsedAtoms);
    }
}
