package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.TreeNode;

public class TreeNodeDTO<T> implements TreeNode<T> {

    private T value;
    private ListNode<TreeNode<T>> children;

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

    static <T> void appendChild(TreeNode<T> tree, TreeNode<T> child) {
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
