package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;

public class LinkDTO implements Link {
    private Topic target;
    private String label;

    @Override
    public void destroy() {
        // nothing to do
    }

    @Override
    public Topic getTarget() {
        return target;
    }

    @Override
    public boolean setTarget(Topic target) {
        this.target = target;
        return false;
    }

    @Override
    public String getLabel() {
        return label;
    }

    @Override
    public boolean setLabel(String label) {
        this.label = label;
        return false;
    }

    @Override
    public Page getContext() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean setContext(Page page) {
        throw new UnsupportedOperationException();
    }
}
