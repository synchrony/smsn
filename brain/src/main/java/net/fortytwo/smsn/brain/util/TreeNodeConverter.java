package net.fortytwo.smsn.brain.util;

import hydra.util.Opt;
import net.fortytwo.smsn.brain.*;
import net.fortytwo.smsn.brain.model.dto.LinkDTO;
import net.fortytwo.smsn.brain.model.dto.PageDTO;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;

import java.util.ArrayList;
import java.util.List;

/**
 * Converts between the old mutable TreeNode<Link> entities and the new immutable TreeNode.
 * This enables gradual migration from the old model to the new simplified model.
 */
public class TreeNodeConverter {

    /**
     * Convert from new immutable TreeNode to old mutable TreeNode<Link>.
     * This is used when we need to work with legacy code that expects TreeNode<Link>.
     */
    public static TreeNode<Link> toTreeNodeLink(net.fortytwo.smsn.brain.TreeNode newNode) {
        TreeNode<Link> oldNode = createTreeNodeLink();

        // Set ID
        TreeViews.setId(oldNode, newNode.id);

        // Set title
        TreeViews.setTitle(oldNode, newNode.title);

        // Set weight
        TreeViews.setWeight(oldNode, newNode.weight.value);

        // Set source
        TreeViews.setSource(oldNode, newNode.source.value);

        // Set created timestamp
        TreeViews.setCreated(oldNode, newNode.created.value);

        // Set optional fields
        if (newNode.priority.isPresent()) {
            TreeViews.setPriority(oldNode, newNode.priority.get().value);
        }

        if (newNode.alias.isPresent()) {
            TreeViews.setAlias(oldNode, newNode.alias.get());
        }

        if (newNode.text.isPresent()) {
            TreeViews.setText(oldNode, newNode.text.get());
        }

        if (newNode.shortcut.isPresent()) {
            TreeViews.setShortcut(oldNode, newNode.shortcut.get());
        }

        // Set counts
        oldNode.setNumberOfChildren(newNode.numberOfChildren);
        oldNode.setNumberOfParents(newNode.numberOfParents);

        // Recursively convert children
        for (net.fortytwo.smsn.brain.TreeNode newChild : newNode.children) {
            oldNode.addChild(toTreeNodeLink(newChild));
        }

        return oldNode;
    }

    /**
     * Helper method to create a new TreeNode<Link> with proper initialization.
     */
    private static TreeNode<Link> createTreeNodeLink() {
        TreeNode<Link> node = new TreeNodeDTO<>();
        Link link = new LinkDTO();
        link.setPage(new PageDTO());
        node.setValue(link);
        return node;
    }

    /**
     * Convert from old mutable TreeNode<Link> to new immutable TreeNode.
     * This is used when we want to serialize or work with the new simplified model.
     */
    public static net.fortytwo.smsn.brain.TreeNode fromTreeNodeLink(TreeNode<Link> oldNode) {
        // Extract required fields
        AtomId id = TreeViews.getId(oldNode);
        if (id == null) {
            id = new AtomId("temp-" + System.nanoTime());
        }

        Long createdValue = TreeViews.getCreated(oldNode);
        Timestamp created = new Timestamp(createdValue != null ? createdValue.intValue() : (int) (System.currentTimeMillis() / 1000));

        Float weightValue = TreeViews.getWeight(oldNode);
        Normed weight = new Normed(weightValue != null ? weightValue : 0.5f);

        String sourceValue = TreeViews.getSource(oldNode);
        SourceName source = new SourceName(sourceValue != null ? sourceValue : "public");

        String title = TreeViews.getTitle(oldNode);
        if (title == null) {
            title = "";
        }

        // Extract optional fields
        Float priorityValue = TreeViews.getPriority(oldNode);
        Opt<Normed> priority = priorityValue != null && priorityValue > 0
                ? Opt.of(new Normed(priorityValue))
                : Opt.empty();

        String aliasValue = TreeViews.getAlias(oldNode);
        Opt<String> alias = aliasValue != null ? Opt.of(aliasValue) : Opt.empty();

        String textValue = TreeViews.getText(oldNode);
        Opt<String> text = textValue != null ? Opt.of(textValue) : Opt.empty();

        String shortcutValue = TreeViews.getShortcut(oldNode);
        Opt<String> shortcut = shortcutValue != null ? Opt.of(shortcutValue) : Opt.empty();

        // Recursively convert children
        List<net.fortytwo.smsn.brain.TreeNode> children = new ArrayList<>();
        if (oldNode.getChildren() != null) {
            for (TreeNode<Link> oldChild : ListNode.toJavaList(oldNode.getChildren())) {
                children.add(fromTreeNodeLink(oldChild));
            }
        }

        int numberOfChildren = oldNode.getNumberOfChildren();
        int numberOfParents = oldNode.getNumberOfParents();

        return new net.fortytwo.smsn.brain.TreeNode(
                id, created, weight, priority, source, title, alias, text, shortcut,
                children, numberOfChildren, numberOfParents
        );
    }
}
