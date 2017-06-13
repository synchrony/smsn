package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.entities.Topic;

public class TopicDTO implements Topic {
    private String id;

    @Override
    public void destroy() {
        // nothing to do
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public void setId(final String id) {
        this.id = id;
    }

    @Override
    public boolean isIsolated() {
        return false;
    }
}
