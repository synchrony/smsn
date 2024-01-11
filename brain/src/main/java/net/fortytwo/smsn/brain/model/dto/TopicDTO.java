package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.model.entities.Topic;

public class TopicDTO implements Topic {
    private AtomId id;

    @Override
    public void destroy() {
        // nothing to do
    }

    @Override
    public AtomId getId() {
        return id;
    }

    @Override
    public void setId(final AtomId id) {
        this.id = id;
    }

    @Override
    public boolean isIsolated() {
        return false;
    }
}
