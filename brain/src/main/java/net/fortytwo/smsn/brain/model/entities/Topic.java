package net.fortytwo.smsn.brain.model.entities;

public interface Topic extends Entity {

    String getId();

    void setId(String id);

    boolean isIsolated();
}
