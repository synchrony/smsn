package net.fortytwo.smsn.brain.model.entities;

import net.fortytwo.smsn.brain.AtomId;

public interface Topic extends Entity {

    AtomId getId();

    void setId(AtomId id);

    boolean isIsolated();
}
