package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Atom;

import java.io.Serializable;

public interface ViewStyle {
    enum Direction {Forward, Backward}

    String getName();

    Iterable<Atom> getLinked(Atom root, Filter filter);

    boolean addOnUpdate();

    boolean deleteOnUpdate();

    Direction getDirection();

    ViewStyle getInverse();
}
