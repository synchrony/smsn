package net.fortytwo.myotherbrain.notes;

import com.tinkerpop.blueprints.pgm.CloseableSequence;
import com.tinkerpop.blueprints.pgm.Index;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.frames.FramesManager;
import com.tinkerpop.tinkubator.pgsail.PropertyGraphSail;
import net.fortytwo.flow.Collector;
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

import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NotesSemantics {

    private final MOBGraph store;
    private final FramesManager manager;
    private final QueryEngine rippleQueryEngine;

    public NotesSemantics(final MOBGraph store) {
        this.store = store;
        this.manager = store.getManager();

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
     * @param root    the key of the root atom of the view
     * @param depth   the depth of the view.
     *                A view of depth 0 contains only the root,
     *                while a view of depth 1 also contains all children of the root,
     *                a view of depth 2 all grandchildren, etc.
     * @param filter  a collection of criteria for atoms and links.
     *                Atoms and links which do not meet the criteria are not to appear in the view.
     * @param inverse whether to produce an inverse view
     * @return a partial view of the graph as a tree of <code>Note</code> objects
     */
    public Note view(final Atom root,
                     final int depth,
                     final Filter filter,
                     final boolean inverse) {
        if (null == root) {
            throw new IllegalArgumentException("null root");
        }

        return viewRecursive(root, depth, filter, inverse);
    }

    /**
     * Updates the graph.
     *
     * @param root     the root of the subgraph to be updated
     * @param children the children of the root atom
     * @param depth    the minimum depth to which the graph will be updated
     * @param filter   a collection of criteria for atoms and links.
     *                 Atoms and links which do not meet the criteria are not to be affected by the update.
     * @param inverse  whether to push an inverse view
     * @throws InvalidUpdateException if the update cannot be performed as specified
     */
    public void update(final Atom root,
                       final List<Note> children,
                       final int depth,
                       final Filter filter,
                       final boolean inverse) throws InvalidUpdateException {
        if (null == root) {
            throw new IllegalArgumentException("null root");
        }

        updateRecursive(root, children, depth, filter, true, inverse);
    }

    /**
     * Performs full text search.
     *
     * @param query   the search query
     * @param depth   depth of the search results view
     * @param filter  a collection of criteria for atoms and links.
     *                Atoms and links which do not meet the criteria are not to appear in search results.
     * @param inverse whether to produce an inverse view
     * @return an ordered list of query results
     */
    public Note search(final String query,
                       final int depth,
                       final Filter filter,
                       final boolean inverse) {

        Note result = new Note();
        result.setTargetValue("full text search results for \"" + query + "\"");

        // TODO: this relies on a temporary Blueprints hack which only works with Neo4j
        CloseableSequence<Vertex> i = store.getGraph().getIndex(Index.VERTICES, Vertex.class).get("value", "%query%" + query);
        try {
            while (i.hasNext()) {
                Atom a = getAtom(i.next());

                if (filter.isVisible(a)) {
                    Note n = view(a, depth - 1, filter, inverse);
                    result.addChild(n);
                }
            }
        } finally {
            i.close();
        }

        Collections.sort(result.getChildren(), new NoteComparator());
        return result;
    }

    /**
     * Performs a Ripple query.
     *
     * @param query   the Ripple query to execute
     * @param depth   depth of the search results view
     * @param filter  a collection of criteria for atoms and links.
     *                Atoms and links which do not meet the criteria are not to appear in search results.
     * @param inverse whether to produce an inverse view
     * @return an ordered list of query results
     * @throws net.fortytwo.ripple.RippleException
     *          if the query fails in Ripple
     */
    public Note rippleQuery(final String query,
                            final int depth,
                            final Filter filter,
                            final boolean inverse) throws RippleException {

        Note result = new Note();
        result.setTargetValue("Ripple results for \"" + query + "\"");

        Collector<RippleList> results = new Collector<RippleList>();
        QueryPipe qp = new QueryPipe(rippleQueryEngine, results);
        try {
            qp.put(query);
        } finally {
            qp.close();
        }

        long now = new Date().getTime();

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
            Atom a = getAtom(vx);

            if (filter.isVisible(a)) {
                Note n = view(a, depth - 1, filter, inverse);
                result.addChild(n);
            }
        }

        Collections.sort(result.getChildren(), new NoteComparator());
        return result;
    }

    private Note viewRecursive(final Atom root,
                               final int depth,
                               final Filter filter,
                               final boolean inverse) {
        if (null == root) {
            throw new IllegalStateException("null view root");
        }

        Note n = new Note();

        n.setTargetValue(root.getValue());
        n.setTargetKey((String) root.asVertex().getId());
        n.setTargetWeight(root.getWeight());
        n.setTargetSharability(root.getSharability());
        n.setTargetCreated(root.getCreated());

        if (depth > 0) {
            for (Atom target : inverse ? root.getInNotes() : root.getOutNotes()) {
                if (filter.isVisible(target)) {
                    Note cn = viewRecursive(target, depth - 1, filter, inverse);
                    n.addChild(cn);
                }
            }

            Collections.sort(n.getChildren(), new NoteComparator());
        }

        return n;
    }

    private void updateRecursive(final Atom root,
                                 final List<Note> children,
                                 final int depth,
                                 final Filter filter,
                                 boolean destructive,
                                 final boolean inverse) throws InvalidUpdateException {
        // Keep adding items beyond the depth of the view, but don't delete items.
        if (0 >= depth) {
            destructive = false;
        }

        Set<String> before = new HashSet<String>();
        for (Note n : viewRecursive(root, 1, filter, inverse).getChildren()) {
            before.add(n.getTargetKey());
        }

        Set<String> after = new HashSet<String>();
        for (Note n : children) {
            String id = n.getTargetKey();

            if (null != id) {
                after.add(n.getTargetKey());
            }
        }

        if (destructive) {
            for (String id : before) {
                if (!after.contains(id)) {
                    Atom target = getAtom(id);

                    if (inverse) {
                        root.removeInNote(target);
                    } else {
                        root.removeOutNote(target);
                    }
                }
            }
        }

        for (Note n : children) {
            String id = n.getTargetKey();

            Atom target;

            if (null == id) {
                target = createAtom(filter);
            } else {
                target = getAtom(id);

                if (null == target) {
                    throw new IllegalStateException("atom with given id '" + id + "' does not exist");
                }
            }

            target.setValue(n.getTargetValue());

            if (!before.contains(id)) {
                if (inverse) {
                    root.addInNote(target);
                } else {
                    root.addOutNote(target);
                }

                updateRecursive(target, n.getChildren(), depth - 1, filter, false, inverse);
            } else {
                updateRecursive(target, n.getChildren(), depth - 1, filter, destructive, inverse);
            }
        }
    }

    public Atom getAtom(final String key) {
        Vertex v = store.getGraph().getVertex(key);

        return null == v ? null : getAtom(v);
    }

    private Atom getAtom(final Vertex v) {
        if (null == v) {
            throw new IllegalArgumentException("null vertex");
        }

        return manager.frame(v, Atom.class);
    }

    private Atom createAtom(final Filter filter) {
        Atom a = manager.frame(store.getGraph().addVertex(null), Atom.class);
        a.setCreated(new Date().getTime());

        a.setSharability(filter.defaultSharability);
        a.setWeight(filter.defaultWeight);

        return a;
    }

    private class NoteComparator implements Comparator<Note> {
        public int compare(Note a, Note b) {
            int cmp = b.getTargetWeight().compareTo(a.getTargetWeight());

            if (0 == cmp) {
                cmp = b.getTargetCreated().compareTo(a.getTargetCreated());
            }

            return cmp;
        }
    }

    public static class InvalidUpdateException extends Exception {
        public InvalidUpdateException(final String message) {
            super(message);
        }
    }
}
