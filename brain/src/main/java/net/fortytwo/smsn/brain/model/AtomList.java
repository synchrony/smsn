package net.fortytwo.smsn.brain.model;

import java.util.List;

public interface AtomList {

    String getId();

    Atom getFirst();

    boolean setFirst(Atom first);

    AtomList getRest();

    boolean setRest(AtomList rest);

    AtomList getRestOf();

    Atom getNotesOf();

    List<Atom> toJavaList();
}
