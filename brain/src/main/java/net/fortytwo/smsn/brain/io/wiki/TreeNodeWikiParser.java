package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.TreeNode;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses wiki-format text into Hydra TreeNode structures.
 * Replaces legacy WikiParser for creating tree hierarchies.
 * Uses stack-of-stacks approach to properly handle siblings at same indent level.
 */
public class TreeNodeWikiParser {

    // Match all bullet types: * (new), + (expanded existing), · (collapsed existing)
    private static final Pattern BULLET_PATTERN = Pattern.compile("^(\\s*)([*+·])\\s+(.*)$");
    private static final Pattern ID_PATTERN = Pattern.compile("^:([^:]+):");
    // Property pattern allows empty values (e.g., "@alias" alone will clear the property)
    private static final Pattern PROPERTY_PATTERN = Pattern.compile("^@(\\w+)\\s*(.*)$");

    // Stack of stacks - each indent level has its own stack of siblings
    private final Stack<Stack<TreeNodeBuilder>> nodeHierarchy = new Stack<>();
    private final Stack<Integer> indentHierarchy = new Stack<>();

    /**
     * Parse wiki-format text into a TreeNode.
     */
    public TreeNode parse(InputStream inputStream) throws IOException {
        reset();

        try (BufferedReader br = new BufferedReader(new InputStreamReader(inputStream, SemanticSynchrony.UTF8))) {
            String line;
            while ((line = br.readLine()) != null) {
                parseLine(line);
            }
        }

        // Finalize by collapsing all levels back to the root
        adjustHierarchy(-1);

        // After adjustHierarchy(-1), the dummy root should have all top-level nodes as children
        // Extract the dummy root and return it directly
        if (!nodeHierarchy.isEmpty() && !nodeHierarchy.peek().isEmpty()) {
            TreeNodeBuilder dummyRoot = nodeHierarchy.peek().peek();
            return dummyRoot.build();
        }

        // Fallback: empty tree
        TreeNodeBuilder emptyRoot = new TreeNodeBuilder();
        emptyRoot.setTitle("");
        return emptyRoot.build();
    }

    private void reset() {
        nodeHierarchy.clear();
        indentHierarchy.clear();
        // Initialize with indent -1 for the root level
        indentHierarchy.push(-1);

        // Create a dummy root node to serve as parent for all top-level nodes
        // This matches the original WikiParser pattern
        Stack<TreeNodeBuilder> rootStack = new Stack<>();
        TreeNodeBuilder dummyRoot = new TreeNodeBuilder();
        dummyRoot.setTitle("");  // Empty title for synthetic root
        rootStack.push(dummyRoot);
        nodeHierarchy.push(rootStack);
    }

    private TreeNodeBuilder currentBuilder = null;

    private void parseLine(String line) {
        String trimmed = line.trim();

        // Skip empty lines
        if (trimmed.isEmpty()) {
            return;
        }

        // Check for property line (@alias, @weight, etc.)
        Matcher propertyMatcher = PROPERTY_PATTERN.matcher(trimmed);
        if (propertyMatcher.matches()) {
            String key = propertyMatcher.group(1);
            String value = propertyMatcher.group(2);

            // Property lines apply to the most recent node
            // If no current builder (top-level properties), apply to dummy root
            if (currentBuilder == null && !nodeHierarchy.isEmpty() && !nodeHierarchy.peek().isEmpty()) {
                nodeHierarchy.peek().peek().setProperty(key, value);
            } else if (currentBuilder != null) {
                currentBuilder.setProperty(key, value);
            }
            return;
        }

        // Check for bullet point
        Matcher bulletMatcher = BULLET_PATTERN.matcher(line);
        if (bulletMatcher.matches()) {
            int indent = bulletMatcher.group(1).length();
            String bulletType = bulletMatcher.group(2);  // *, +, or ·
            String content = bulletMatcher.group(3);

            // Adjust hierarchy based on indentation
            adjustHierarchy(indent);

            // Parse the content
            TreeNodeBuilder builder = new TreeNodeBuilder();
            currentBuilder = builder;  // Track for property lines

            // Extract ID if present
            Matcher idMatcher = ID_PATTERN.matcher(content);
            if (idMatcher.find()) {
                String id = idMatcher.group(1);
                builder.setId(new AtomId(id));
                content = content.substring(idMatcher.end()).trim();
            }

            // Remaining content is the title
            if (!content.isEmpty()) {
                builder.setTitle(content);
            }

            // Add to current level's sibling stack
            nodeHierarchy.peek().push(builder);
        }
    }

    private void adjustHierarchy(int newIndent) {
        // Collapse levels until we're at or above the target indent
        while (indentHierarchy.peek() > newIndent) {
            indentHierarchy.pop();
            Stack<TreeNodeBuilder> siblings = nodeHierarchy.pop();
            TreeNodeBuilder parent = nodeHierarchy.peek().peek();

            // Add all siblings as children to parent (in order)
            List<TreeNode> children = new ArrayList<>();
            while (!siblings.isEmpty()) {
                children.add(0, siblings.pop().build());  // Prepend to maintain order
            }
            for (TreeNode child : children) {
                parent.addChild(child);
            }
        }

        // If we need a new level, create it
        if (newIndent > indentHierarchy.peek()) {
            indentHierarchy.push(newIndent);
            nodeHierarchy.push(new Stack<>());
        }
    }

    /**
     * Mutable builder for creating TreeNodes during parsing.
     */
    private static class TreeNodeBuilder {
        private AtomId id;
        private String title = "";
        private final List<TreeNode> children = new ArrayList<>();
        private final Map<String, String> properties = new HashMap<>();

        public void setId(AtomId id) {
            this.id = id;
        }

        public void setTitle(String title) {
            this.title = title;
        }

        public void addChild(TreeNode child) {
            children.add(child);
        }

        public void setProperty(String key, String value) {
            properties.put(key, value);
        }

        public TreeNode build() {
            // Use defaults for missing properties
            // Use "temp-" prefix for new atoms (to be created during update)
            AtomId atomId = id != null ? id : new AtomId("temp-" + System.nanoTime());
            long now = System.currentTimeMillis();

            // Parse properties from @property lines
            // Empty values (e.g., "@weight" alone) use defaults or clear the property
            String weightStr = properties.get("weight");
            float weight = (weightStr != null && !weightStr.isEmpty())
                ? Float.parseFloat(weightStr)
                : SemanticSynchrony.DEFAULT_WEIGHT;

            String priorityStr = properties.get("priority");
            hydra.util.Opt<net.fortytwo.smsn.brain.Normed> priority;
            if (priorityStr != null && !priorityStr.isEmpty()) {
                priority = hydra.util.Opt.of(new net.fortytwo.smsn.brain.Normed(Float.parseFloat(priorityStr)));
            } else if (priorityStr != null) {
                // Empty string means "clear priority" - use empty Opt with marker value
                // We use a special sentinel value to distinguish "not specified" from "explicitly cleared"
                priority = hydra.util.Opt.of(new net.fortytwo.smsn.brain.Normed(-1.0f));
            } else {
                priority = hydra.util.Opt.empty();
            }

            String source = properties.getOrDefault("source", "public");

            // For string properties, empty string means "clear the property"
            String aliasStr = properties.get("alias");
            hydra.util.Opt<String> alias = aliasStr != null
                ? hydra.util.Opt.of(aliasStr)  // Empty string is valid - means "clear"
                : hydra.util.Opt.empty();

            String textStr = properties.get("text");
            hydra.util.Opt<String> text = textStr != null
                ? hydra.util.Opt.of(textStr)  // Empty string is valid - means "clear"
                : hydra.util.Opt.empty();

            String shortcutStr = properties.get("shortcut");
            hydra.util.Opt<String> shortcut = shortcutStr != null
                ? hydra.util.Opt.of(shortcutStr)  // Empty string is valid - means "clear"
                : hydra.util.Opt.empty();

            return new TreeNode(
                    atomId,
                    new net.fortytwo.smsn.brain.Timestamp(now),
                    new net.fortytwo.smsn.brain.Normed(weight),
                    priority,
                    new net.fortytwo.smsn.brain.SourceName(source),
                    title,
                    alias,
                    text,
                    shortcut,
                    children,
                    children.size(),
                    0 // numberOfParents unknown during parsing
            );
        }
    }
}
