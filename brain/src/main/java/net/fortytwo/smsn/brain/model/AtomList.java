package net.fortytwo.smsn.brain.model;

import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
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
