package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.TreeNode;

public class TreeNodeDTO<T> implements TreeNode<T> {

    private T value;
    private ListNode<TreeNode<T>> children;
    private int numberOfChildren;
    private int numberOfParents;

    @Override
    public void destroy() {
        // nothing to do
    }

    @Override
    public T getValue() {
        return value;
    }

    @Override
    public void setValue(T value) {
        this.value = value;
    }

    @Override
    public ListNode<TreeNode<T>> getChildren() {
        return children;
    }

    @Override
    public void setChildren(ListNode<TreeNode<T>> children) {
        this.children = children;
    }

    // TODO: use of addChild is currently inefficient.  Use stack rather than queue semantics.
    @Override
    public void addChild(TreeNode<T> child) {
        appendChild(this, child);
    }

    @Override
    public int getNumberOfChildren() {
        return numberOfChildren;
    }

    @Override
    public void setNumberOfChildren(int numberOfChildren) {
        this.numberOfChildren = numberOfChildren;
    }

    @Override
    public int getNumberOfParents() {
        return numberOfParents;
    }

    @Override
    public void setNumberOfParents(int numberOfParents) {
        this.numberOfParents = numberOfParents;
    }

    // TODO: temporary
    public static TreeNode<Link> createEmptyNode() {
        TreeNode<Link> node = new TreeNodeDTO<>();
        Link link = new LinkDTO();
        node.setValue(link);
        Page page = new PageDTO();
        link.setPage(page);
        return node;
    }

    private static <T> void appendChild(TreeNode<T> tree, TreeNode<T> child) {
        ListNode<TreeNode<T>> toInsert = new ListNodeDTO<>(child, null);

        ListNode<TreeNode<T>> cur = tree.getChildren();
        ListNode<TreeNode<T>> prev = null;
        while (null != cur) {
            prev = cur;
            cur = cur.getRest();
        }

        if (null == prev) {
            tree.setChildren(toInsert);
        } else {
            prev.setRest(toInsert);
        }
    }
}
