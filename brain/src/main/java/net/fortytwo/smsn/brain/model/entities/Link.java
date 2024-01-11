package net.fortytwo.smsn.brain.model.entities;

import net.fortytwo.smsn.brain.model.Role;

public interface Link extends Entity {

    Role getRole();

    void setRole(Role role);

    Topic getTarget();

    void setTarget(Topic target);

    String getLabel();

    void setLabel(String label);

    Page getPage();

    void setPage(final Page page);
}
