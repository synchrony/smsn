package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.Tag;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;

public class LinkDTO implements Link {

    private Tag tag;
    private Topic target;
    private String label;
    private Page page;

    @Override
    public void destroy() {
        // nothing to do
    }

    public Tag getTag() {
        return tag;
    }

    public void setTag(Tag tag) {
        this.tag = tag;
    }

    @Override
    public Topic getTarget() {
        return target;
    }

    @Override
    public void setTarget(Topic target) {
        this.target = target;
    }

    @Override
    public String getLabel() {
        return label;
    }

    @Override
    public void setLabel(String label) {
        this.label = label;
    }

    @Override
    public Page getPage() {
        return page;
    }

    @Override
    public void setPage(Page page) {
        this.page = page;
    }
}
