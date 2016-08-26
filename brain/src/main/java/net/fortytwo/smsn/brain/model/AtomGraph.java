package net.fortytwo.smsn.brain.model;

import java.util.List;

/**
 * A graph of atoms and lists conforming to the Extend-o-Brain data model
 */
public interface AtomGraph {

    /**
     * The configurable namespace into which things, i.e. classified atoms, are mapped
     */
    static final String PROP_THING_NAMESPACE = "net.fortytwo.smsn.brain.thingNamespace";

    static final String DEFAULT_THING_NAMESPACE = "http://example.org/things/";

    Iterable<Atom> getAllAtoms();

    Atom getAtom(String id);

    List<Atom> getAtomsByShortcut(String shortcut, Filter filter);

    List<Atom> getAtomsByValue(String value, Filter filter);

    List<Atom> getAtomsByAcronym(String acronym, Filter filter);

    String idOfAtom(Atom a);

    String iriOfAtom(Atom a);

    Atom createAtom(Filter filter, String id);

    AtomList createAtomList(String id);

    AtomList createAtomList(Atom... elements);

    void removeIsolatedAtoms(Filter filter);

    void notifyOfUpdate();

    void reindexAtom(Atom a);

    long getLastUpdate();

    void commit();

    AtomGraph createFilteredGraph(Filter filter);
}
