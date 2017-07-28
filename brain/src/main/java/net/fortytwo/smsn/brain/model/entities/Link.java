package net.fortytwo.smsn.brain.model.entities;

import net.fortytwo.smsn.brain.model.Tag;

public interface Link extends Entity {

    Tag getTag();

    void setTag(Tag tag);

    Topic getTarget();

    void setTarget(Topic target);

    String getLabel();

    void setLabel(String label);

    Page getPage();

    void setPage(final Page page);
}
