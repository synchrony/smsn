package net.fortytwo.smsn.brain.model.entities;

import java.util.function.Consumer;

public interface Topic extends Entity {

    String getId();

    boolean setId(String id);

    Page getPrimaryTopicOf();

    void forTargetOf(Consumer<Link> consumer);

    boolean isIsolated();
}
