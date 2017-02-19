package net.fortytwo.smsn.brain.model.entities;

public interface Link extends Entity {

    Topic getTarget();

    boolean setTarget(Topic target);

    String getLabel();

    boolean setLabel(String label);

    Page getContext();

    boolean setContext(Page page);
}
