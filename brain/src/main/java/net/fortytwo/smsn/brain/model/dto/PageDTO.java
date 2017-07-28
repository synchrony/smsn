package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.Tag;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;

public class PageDTO implements Page {

    public static final String TRANSITIONAL_ID = "IGNORED";
    public static final String TRANSITIONAL_TITLE = "IGNORED";

    private Tag tag;
    private String text;
    private String source;
    private String alias;
    private String shortcut;
    private Float weight;
    private Float priority;

    // TODO: temporary
    private Long created;

    private TreeNode<Link> content;

    @Override
    public void destroy() {
        // nothing to do
    }

    public Tag getTag() {
        return tag;
    }

    public void setTag(final Tag tag) {
        this.tag = tag;
    }


    @Override
    public String getText() {
        return text;
    }

    @Override
    public void setText(String text) {
        this.text = text;
    }

    @Override
    public String getAlias() {
        return alias;
    }

    @Override
    public void setAlias(String alias) {
        this.alias = alias;
    }

    @Override
    public String getSource() {
        return source;
    }

    @Override
    public void setSource(String source) {
        this.source = source;
    }

    @Override
    public String getShortcut() {
        return shortcut;
    }

    @Override
    public void setShortcut(String shortcut) {
        this.shortcut = shortcut;
    }

    @Override
    public TreeNode<Link> getContent() {
        return content;
    }

    @Override
    public void setContent(TreeNode<Link> tree) {
        content = tree;
    }

    @Override
    public Float getWeight() {
        return weight;
    }

    @Override
    public void setWeight(Float weight) {
        this.weight = weight;
    }

    @Override
    public Float getPriority() {
        return priority;
    }

    @Override
    public void setPriority(Float priority) {
        this.priority = priority;
    }

    @Override
    public Long getCreated() {
        return created;
    }

    @Override
    public void setCreated(Long created) {
        this.created = created;
    }

    public static Page createTransitional() {
        Page page = new PageDTO();
        Link link = new LinkDTO();
        Topic topic = new TopicDTO();
        topic.setId(TRANSITIONAL_ID);
        link.setTarget(topic);
        link.setLabel(TRANSITIONAL_TITLE);
        TreeNode<Link> content = new TreeNodeDTO<>();
        content.setValue(link);
        page.setContent(content);
        return page;
    }
}
