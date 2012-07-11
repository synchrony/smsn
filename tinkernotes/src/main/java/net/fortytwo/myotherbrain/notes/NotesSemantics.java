package net.fortytwo.myotherbrain.notes;

import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.tinkubator.pgsail.PropertyGraphSail;
import net.fortytwo.flow.Collector;
import net.fortytwo.myotherbrain.ActivityLog;
import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.MOBGraph;
import net.fortytwo.ripple.Ripple;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.Model;
import net.fortytwo.ripple.model.RippleList;
import net.fortytwo.ripple.model.impl.sesame.SesameModel;
import net.fortytwo.ripple.query.QueryEngine;
import net.fortytwo.ripple.query.QueryPipe;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.sail.Sail;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NotesSemantics {

    private final MOBGraph store;
    private final QueryEngine rippleQueryEngine;

    public NotesSemantics(final MOBGraph store) {
        this.store = store;

        try {
            Ripple.initialize();

            Sail sail = new PropertyGraphSail(store.getGraph());
            sail.initialize();

            //sail = new RecorderSail(sail, System.out);

            Model rippleModel = new SesameModel(sail);
            rippleQueryEngine = new QueryEngine(rippleModel);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * Generates a view of the graph.
     *
     * @param root   the key of the root atom of the view
     * @param depth  the depth of the view.
     *               A view of depth 0 contains only the root,
     *               while a view of depth 1 also contains all children of the root,
     *               a view of depth 2 all grandchildren, etc.
     * @param filter a collection of criteria for atoms and links.
     *               Atoms and links which do not meet the criteria are not to appear in the view.
     * @param style  the adjacency style of the view
     * @return a partial view of the graph as a tree of <code>Note</code> objects
     */
    public Note view(final Atom root,
                     final int depth,
                     final Filter filter,
                     final AdjacencyStyle style,
                     final ActivityLog log) {
        if (null != log) {
            log.logView(root);
        }

        return viewInternal(root, null, depth, filter, style);
    }

    private Note viewInternal(final Atom root,
                              final Atom parent,
                              final int depth,
                              final Filter filter,
                              final AdjacencyStyle style) {
        if (null == root) {
            throw new IllegalStateException("null view root");
        }

        Note n = toNote(root);

        if (depth > 0) {
            for (Atom target : style.getLinked(root, parent)) {
                if (filter.isVisible(target)) {
                    Note cn = viewInternal(target, root, depth - 1, filter, style);
                    n.addChild(cn);
                }
            }

            Collections.sort(n.getChildren(), new NoteComparator());
        }

        return n;
    }

    public Note customView(final List<String> atomIds,
                           final Filter filter) {
        Note n = new Note();

        for (String id : atomIds) {
            Atom a = store.getAtom(id);
            if (null == a) {
                throw new IllegalArgumentException("no such atom: " + id);
            }

            n.addChild(viewInternal(a, null, 0, filter, FORWARD_DIRECTED_ADJACENCY));
        }

        return n;
    }

    /**
     * Updates the graph.
     *
     * @param root        the root of the subgraph to be updated
     * @param children    the children of the root atom
     * @param depth       the minimum depth to which the graph will be updated
     * @param filter      a collection of criteria for atoms and links.
     *                    Atoms and links which do not meet the criteria are not to be affected by the update.
     * @param destructive whether to remove items which do not appear in the update view
     * @param style       the adjacency style of the view
     * @throws InvalidUpdateException if the update cannot be performed as specified
     */
    public void update(final Atom root,
                       final List<Note> children,
                       final int depth,
                       final Filter filter,
                       boolean destructive,
                       final AdjacencyStyle style,
                       final ActivityLog log) throws InvalidUpdateException {
        updateInternal(root, null, children, depth, filter, destructive, style, log);
    }

    public void updateInternal(final Atom root,
                               final Atom parent,
                               final List<Note> children,
                               final int depth,
                               final Filter filter,
                               boolean destructive,
                               final AdjacencyStyle style,
                               final ActivityLog log) throws InvalidUpdateException {
        if (null == root) {
            throw new IllegalStateException("null view root");
        }

        // Keep adding items beyond the depth of the view, but don't delete items.
        if (0 >= depth) {
            destructive = false;
        }

        Set<String> before = new HashSet<String>();
        for (Note n : viewInternal(root, parent, 1, filter, style).getChildren()) {
            before.add(n.getId());
        }

        Set<String> after = new HashSet<String>();
        for (Note n : children) {
            String id = n.getId();

            if (null != id) {
                after.add(n.getId());
            }
        }

        if (destructive) {
            for (String id : before) {
                if (!after.contains(id)) {
                    Atom target = store.getAtom(id);

                    style.unlink(root, target, log);
                }
            }
        }

        for (Note n : children) {
            String id = n.getId();

            Atom target;

            if (null == id) {
                target = createAtom(filter, log);
            } else {
                target = store.getAtom(id);

                if (null == target) {
                    throw new IllegalStateException("atom with given id '" + id + "' does not exist");
                }
            }

            setValue(target, n.getValue(), log);

            if (!before.contains(id)) {
                style.link(root, target, log);

                updateInternal(target, root, n.getChildren(), depth - 1, filter, false, style, log);
            } else {
                updateInternal(target, root, n.getChildren(), depth - 1, filter, destructive, style, log);
            }
        }
    }

    /**
     * Performs full text search.
     *
     * @param query  the search query
     * @param depth  depth of the search results view
     * @param filter a collection of criteria for atoms and links.
     *               Atoms and links which do not meet the criteria are not to appear in search results.
     * @param style  the adjacency style of the view
     * @return an ordered list of query results
     */
    public Note search(final String query,
                       final int depth,
                       final Filter filter,
                       final AdjacencyStyle style) {

        Note result = new Note();
        result.setValue("full text search results for \"" + query + "\"");

        for (Atom a : store.getAtomsByFulltextQuery(query, filter)) {
            Note n = viewInternal(a, null, depth - 1, filter, style);
            result.addChild(n);
        }

        Collections.sort(result.getChildren(), new NoteComparator());
        return result;
    }

    /**
     * Performs a Ripple query.
     *
     * @param query  the Ripple query to execute
     * @param depth  depth of the search results view
     * @param filter a collection of criteria for atoms and links.
     *               Atoms and links which do not meet the criteria are not to appear in search results.
     * @param style  the adjacency style of the view
     * @return an ordered list of query results
     * @throws net.fortytwo.ripple.RippleException
     *          if the query fails in Ripple
     */
    public Note rippleQuery(final String query,
                            final int depth,
                            final Filter filter,
                            final AdjacencyStyle style) throws RippleException {

        Note result = new Note();
        result.setValue("Ripple results for \"" + query + "\"");

        Collector<RippleList> results = new Collector<RippleList>();
        QueryPipe qp = new QueryPipe(rippleQueryEngine, results);
        try {
            qp.put(query);
        } finally {
            qp.close();
        }

        Set<Vertex> vertices = new HashSet<Vertex>();

        for (RippleList l : results) {
            System.out.println("result list: " + l);
            if (1 == l.length()) {
                Value v = l.getFirst().toRDF(qp.getConnection()).sesameValue();
                if (v instanceof URI && v.stringValue().startsWith(PropertyGraphSail.VERTEX_NS)) {
                    String s = v.stringValue();

                    if (s.startsWith(PropertyGraphSail.VERTEX_NS)) {
                        Vertex vx = store.getGraph().getVertex(s.substring(PropertyGraphSail.VERTEX_NS.length()));
                        vertices.add(vx);
                    }
                }
            }
        }

        for (Vertex vx : vertices) {
            Atom a = store.getAtom(vx);

            if (filter.isVisible(a)) {
                Note n = viewInternal(a, null, depth - 1, filter, style);
                result.addChild(n);
            }
        }

        Collections.sort(result.getChildren(), new NoteComparator());
        return result;
    }

    private Atom createAtom(final Filter filter,
                            final ActivityLog log) {
        Atom a = store.createAtom(filter);

        if (null != log) {
            log.logCreate(a);
        }

        return a;
    }

    private void setValue(final Atom target,
                          final String value,
                          final ActivityLog log) {
        if (null != log) {
            String prev = target.getValue();
            if (null == prev ? null != value : (null == value || !prev.equals(value))) {
                log.logUpdate(target);
            }
        }

        target.setValue(value);
        store.indexForSearch(target, value);
    }

    private Note toNote(final Atom a) {
        Note n = new Note();

        n.setValue(a.getValue());
        n.setId((String) a.asVertex().getId());
        n.setWeight(a.getWeight());
        n.setSharability(a.getSharability());
        n.setCreated(a.getCreated());
        n.setAlias(a.getAlias());

        return n;
    }

    private class NoteComparator implements Comparator<Note> {
        public int compare(Note a, Note b) {
            int cmp = b.getWeight().compareTo(a.getWeight());

            if (0 == cmp) {
                cmp = b.getCreated().compareTo(a.getCreated());
            }

            return cmp;
        }
    }

    public static class InvalidUpdateException extends Exception {
        public InvalidUpdateException(final String message) {
            super(message);
        }
    }

    public interface AdjacencyStyle {
        String getName();

        Iterable<Atom> getLinked(Atom root, Atom parent);

        void link(Atom source, Atom target, ActivityLog log);

        void unlink(Atom source, Atom target, ActivityLog log);
    }

    public static AdjacencyStyle lookupStyle(final String name) {
        if (name.equals(FORWARD_DIRECTED_ADJACENCY.getName())) {
            return FORWARD_DIRECTED_ADJACENCY;
        } else if (name.equals(BACKWARD_DIRECTED_ADJACENCY.getName())) {
            return BACKWARD_DIRECTED_ADJACENCY;
        } else if (name.equals(UNDIRECTED_ADJACENCY.getName())) {
            return UNDIRECTED_ADJACENCY;
        } else {
            throw new IllegalArgumentException("unknown view style: " + name);
        }
    }

    private static void addOutNote(final Atom root,
                                   final Atom other,
                                   final ActivityLog log) {
        root.addOutNote(other);

        if (null != log) {
            log.logLink(root, other);
        }
    }

    private static void removeOutNote(final Atom root,
                                      final Atom other,
                                      final ActivityLog log) {
        root.removeOutNote(other);

        if (null != log) {
            log.logUnlink(root, other);
        }
    }

    private static void addInNote(final Atom root,
                                  final Atom other,
                                  final ActivityLog log) {
        addOutNote(other, root, log);
    }

    private static void removeInNote(final Atom root,
                                     final Atom other,
                                     final ActivityLog log) {
        removeOutNote(other, root, log);
    }

    public static final AdjacencyStyle FORWARD_DIRECTED_ADJACENCY = new AdjacencyStyle() {
        public String getName() {
            return "directed-forward";
        }

        public Iterable<Atom> getLinked(Atom root, Atom parent) {
            return root.getOutNotes();
        }

        public void link(Atom source, Atom target, ActivityLog log) {
            addOutNote(source, target, log);
        }

        public void unlink(Atom source, Atom target, ActivityLog log) {
            removeOutNote(source, target, log);
        }
    };

    public static final AdjacencyStyle BACKWARD_DIRECTED_ADJACENCY = new AdjacencyStyle() {
        public String getName() {
            return "directed-backward";
        }

        public Iterable<Atom> getLinked(Atom root, Atom parent) {
            return root.getInNotes();
        }

        public void link(Atom source, Atom target, ActivityLog log) {
            addInNote(source, target, log);
        }

        public void unlink(Atom source, Atom target, ActivityLog log) {
            removeInNote(source, target, log);
        }
    };

    public static final AdjacencyStyle UNDIRECTED_ADJACENCY = new AdjacencyStyle() {
        public String getName() {
            return "undirected";
        }

        public Iterable<Atom> getLinked(Atom root, Atom parent) {
            Collection<Atom> l = new LinkedList<Atom>();
            for (Atom a : root.getInNotes()) {
                if (null == parent || !parent.equals(a)) {
                    l.add(a);
                }
            }
            for (Atom a : root.getOutNotes()) {
                if (null == parent || !parent.equals(a)) {
                    l.add(a);
                }
            }
            return l;
        }

        public void link(Atom source, Atom target, ActivityLog log) {
            // Do an extra check here in case a link is being added merely because it was
            // ommitted from the view (due to symmetry).
            if (!contains(source.getOutNotes(), target)) {
                addOutNote(source, target, log);
            }
        }

        private boolean contains(Iterable<Atom> i,
                                 final Atom a) {
            for (Atom b : i) {
                if (b.equals(a)) {
                    return true;
                }
            }

            return false;
        }

        public void unlink(Atom source, Atom target, ActivityLog log) {
            removeInNote(source, target, log);
            removeOutNote(source, target, log);
        }
    };
}
