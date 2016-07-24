package net.fortytwo.smsn.brain.model;

import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.util.wrappers.id.IdGraph;
import net.fortytwo.smsn.brain.Filter;

import java.util.List;

/**
 * A graph of atoms and lists conforming to the Extend-o-Brain data model
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface AtomGraph {

    /**
     * The configurable namespace into which things, i.e. classified atoms, are mapped
     */
    static final String PROP_THING_NAMESPACE = "net.fortytwo.smsn.brain.thingNamespace";

    static final String DEFAULT_THING_NAMESPACE = "http://example.org/things/";

    IdGraph<KeyIndexableGraph> getPropertyGraph();

    Iterable<Atom> getAllAtoms();

    String idOfAtom(Atom a);

    String iriOfAtom(Atom a);

    Vertex getVertex(String id);

    Atom getAtom(Vertex v);

    Atom getAtom(String id);

    Atom createAtom(Filter filter, String id);

    AtomList createAtomList(Atom... elements);

    void removeIsolatedAtoms(Filter filter);

    void notifyOfUpdate();

    List<Atom> getAtomsWithShortcut(String shortcut, Filter filter);

    List<Atom> getAtomsByFulltextQuery(String query, Filter filter);

    List<Atom> getAtomsByAcronymQuery(String query, Filter filter);

    void addAtomToIndices(Atom a);

    long getLastUpdate();
}
