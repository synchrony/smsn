package net.fortytwo.smsn.brain.model.entities;

import net.fortytwo.smsn.brain.model.Role;

public interface Link extends Node {

    Role getRole();

    void setRole(Role role);

    Topic getTarget();

    void setTarget(Topic target);

    String getLabel();

    void setLabel(String label);
}
