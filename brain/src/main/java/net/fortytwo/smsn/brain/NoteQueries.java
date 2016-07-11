package net.fortytwo.smsn.brain;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.blueprints.Edge;
import com.tinkerpop.blueprints.Vertex;
import net.fortytwo.smsn.brain.rdf.KnowledgeBase;
import net.fortytwo.smsn.brain.util.ListDiff;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteQueries {

    protected static final Logger logger = Logger.getLogger(NoteQueries.class.getName());

    public enum QueryType {
        FullText, Acronym, Shortcut
    }

    private final ExtendoBrain brain;
    //private final QueryEngine rippleQueryEngine;

    /**
     * @param brain the Extend-o-Brain instance to query and update
     */
    public NoteQueries(final ExtendoBrain brain) {
        if (null == brain) {
            throw new IllegalArgumentException();
        }

        this.brain = brain;

        /*
        try {
            Ripple.initialize();

            Sail sail = new PropertyGraphSail(graph.getPropertyGraph());
            sail.initialize();

            //sail = new RecorderSail(sail, System.out);

            Model rippleModel = new SesameModel(sail);
            rippleQueryEngine = new QueryEngine(rippleModel);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }*/
    }

    /**
     * Generates a view of the graph.
     *
     * @param root   the root atom of the view
     * @param height the maximum height of the view.
     *               A view of height 0 contains only the root,
     *               while a view of height 1 also contains all children of the root,
     *               a view of height 2 all grandchildren, etc.
     * @param filter a collection of criteria for atoms and links.
     *               Atoms and links which do not meet the criteria are not to appear in the view.
     * @param style  the adjacency style of the view
     * @return a partial view of the graph as a tree of <code>Note</code> objects
     */
    public Note view(final Atom root,
                     final int height,
                     final Filter filter,
                     final ViewStyle style) {
        if (null == root || height < 0 || null == filter || null == style) {
            throw new IllegalArgumentException();
        }

        if (null != brain.getActivityLog()) {
            brain.getActivityLog().logView(root);
        }

        return viewInternal(root, height, filter, style);
    }

    private Note viewInternal(final Atom root,
                              final int height,
                              final Filter filter,
                              final ViewStyle style) {
        if (null == root) {
            throw new IllegalStateException("null view root");
        }

        Note n = toNote(root, filter.isVisible(root.asVertex()));

        if (height > 0) {
            for (Atom target : style.getLinked(root, filter)) {
                int h = filter.isVisible(target.asVertex()) ? height - 1 : 0;
                Note cn = viewInternal(target, h, filter, style);
                n.addChild(cn);
            }
        } else {
            if (hasChildren(root, filter, style)) {
                n.setHasChildren(true);
            }
        }

        return n;
    }

    public Note customView(final List<String> atomIds,
                           final Filter filter) {
        if (null == atomIds || null == filter) {
            throw new IllegalArgumentException();
        }

        Note n = new Note();

        for (String id : atomIds) {
            Atom a = brain.getBrainGraph().getAtom(id);
            if (null == a) {
                throw new IllegalArgumentException("no such atom: " + id);
            }

            n.addChild(viewInternal(a, 0, filter, forwardViewStyle));
        }

        return n;
    }

    /**
     * Updates the graph.
     *
     * @param rootNote the root of the note tree
     * @param height   the maximum height of the tree which will be applied to the graph as an update.
     *                 If height is 0, only the root node will be affected,
     *                 while a height of 1 will also affect children (which have a depth of 1 from the root), etc.
     * @param filter   a collection of criteria for atoms and links.
     *                 Atoms and links which do not meet the criteria are not to be affected by the update.
     * @param style    the adjacency style of the view
     * @throws InvalidUpdateException if the update cannot be performed as specified
     */
    public void update(final Note rootNote,
                       final int height,
                       final Filter filter,
                       final ViewStyle style) throws InvalidUpdateException {

        if (null == rootNote || height < 0 || null == filter || null == style) {
            throw new IllegalArgumentException();
        }

        if (!style.addOnUpdate() && !style.deleteOnUpdate()) {
            throw new IllegalStateException("can't update in style " + style);
        }

        updateInternal(rootNote, height, filter, style);

        brain.getBrainGraph().updated();
    }

    private final Comparator<Note> noteComparator = new Comparator<Note>() {
        public int compare(final Note a,
                           final Note b) {
            return null == a.getId()
                    ? (null == b.getId() ? 0 : -1)
                    : (null == b.getId() ? 1 : a.getId().compareTo(b.getId()));
        }
    };

    private boolean hasChildren(final Atom root,
                                final Filter filter,
                                final ViewStyle style) {
        // If the note is invisible, we can't see whether it has children.
        if (!filter.isVisible(root.asVertex())) {
            return false;
        }

        // If the note is visible, we can see its children (although we will not be able to read the values of any
        // children which are themselves invisible).
        Iterable<Atom> children = style.getLinked(root, filter);
        return children.iterator().hasNext();
    }

    private void updateInternal(final Note rootNote,
                                final int height,
                                final Filter filter,
                                final ViewStyle style) throws InvalidUpdateException {

        final Set<String> added = new HashSet<>();
        final Set<String> created = new HashSet<>();

        final Atom root = null == rootNote.getId() ? null : brain.getBrainGraph().getAtom(rootNote.getId());
        if (null != root) {
            setProperties(root, rootNote);

            if (0 >= height || !filter.isVisible(root.asVertex())) {
                return;
            }

            ListDiff.DiffEditor<Note> editor = new ListDiff.DiffEditor<Note>() {
                public void add(final int position,
                                final Note note) throws InvalidUpdateException {
                    if (!style.addOnUpdate()) {
                        return;
                    }

                    // retrieve or create an atom for the note
                    // atoms are only created if they appear under a parent which is also an atom
                    Atom a = getAtom(note);
                    if (null == a) {
                        a = createAtom(note.getId(), filter);
                        created.add((String) a.asVertex().getId());
                    }
                    added.add((String) a.asVertex().getId());
                    if (null == note.getId()) {
                        note.setId((String) a.asVertex().getId());
                    }

                    // create a list node for the atom and insert it
                    AtomList l = brain.getBrainGraph().createAtomList();
                    l.setFirst(a);
                    if (0 == position) {
                        l.setRest(root.getNotes());
                        root.setNotes(l);
                    } else {
                        AtomList prev = root.getNotes();
                        for (int i = 1; i < position; i++) {
                            prev = prev.getRest();
                        }

                        l.setRest(prev.getRest());
                        prev.setRest(l);
                    }

                    // log this activity
                    if (null != brain.getActivityLog()) {
                        brain.getActivityLog().logLink(root, a);
                    }
                }

                public void delete(final int position,
                                   final Note note) {
                    if (!style.deleteOnUpdate()) {
                        return;
                    }

                    AtomList n = root.getNotes();

                    // remove the atom's list node
                    if (0 == position) {
                        root.setNotes(n.getRest());

                        brain.getBrainGraph().deleteListNode(n);
                    } else {
                        AtomList prev = n;
                        for (int i = 1; i < position; i++) {
                            prev = prev.getRest();
                        }

                        AtomList l = prev.getRest();
                        prev.setRest(l.getRest());
                        brain.getBrainGraph().deleteListNode(l);
                    }

                    // log this activity
                    if (null != brain.getActivityLog()) {
                        Atom a = brain.getBrainGraph().getAtom(note.getId());
                        brain.getActivityLog().logUnlink(root, a);
                    }
                }
            };

            List<Note> before = viewInternal(root, 1, filter, style).getChildren();
            List<Note> after = rootNote.getChildren();
            List<Note> lcs = ListDiff.longestCommonSubsequence(before, after, noteComparator);
            ListDiff.applyDiff(before, after, lcs, noteComparator, editor);
        }

        for (Note n : rootNote.getChildren()) {
            // upon adding children:
            // for a child which is a newly created atom, also add grandchildren to one level, possibly recursively
            // if a new child is a new atom, only update the child, not the grandchildren
            // if a child is not new, update both the child and the grandchildren with decreasing height
            int h = null == rootNote.getId()
                    ? height - 1
                    : created.contains(n.getId())
                    ? 1
                    : added.contains(n.getId())
                    ? 0
                    : height - 1;

            // TODO: verify that this can result in multiple log events per call to update()
            updateInternal(n, h, filter, style);
        }
    }

    private Atom getAtom(final Note n) {
        String id = n.getId();
        return null == id ? null : brain.getBrainGraph().getAtom(id);
    }

    private Atom createAtom(final String id,
                            final Filter filter) {
        Atom a = brain.getBrainGraph().createAtom(filter, id);

        if (null != brain.getActivityLog()) {
            brain.getActivityLog().logCreate(a);
        }

        return a;
    }

    /**
     * Performs a specified type of search, such as full text or acronym search
     *
     * @param queryType the type of search to perform
     * @param query     the search query
     * @param height    maximum height of the search results view.
     *                  This must be at least 1, indicating a results node with search results as children.
     *                  A height of 2 includes the children of the results, as well.
     * @param filter    a collection of criteria for atoms and links.
     *                  Atoms and links which do not meet the criteria are not to appear in search results.
     * @param style     the adjacency style of the view
     * @return an ordered list of query results
     */
    public Note search(final QueryType queryType,
                       final String query,
                       final int height,
                       final Filter filter,
                       final ViewStyle style) {
        if (null == query || height < 1 || null == filter || null == style) {
            throw new IllegalArgumentException();
        }

        Note result = new Note();

        List<Atom> results;
        switch (queryType) {
            case FullText:
                results = brain.getBrainGraph().getAtomsByFulltextQuery(query, filter);
                break;
            case Acronym:
                results = brain.getBrainGraph().getAtomsByAcronymQuery(query, filter);
                break;
            case Shortcut:
                results = brain.getBrainGraph().getAtomsWithShortcut(query, filter);
                break;
            default:
                throw new IllegalStateException("unexpected query type: " + queryType);
        }

        for (Atom a : results) {
            Note n = viewInternal(a, height - 1, filter, style);
            result.addChild(n);
        }

        Collections.sort(result.getChildren(), new NoteComparator());

        result.setValue(queryType.name() + " results for \"" + query + "\"");
        return result;
    }

    public Note findRoots(final Filter filter,
                          final ViewStyle style,
                          final int height) {
        if (null == filter || null == style || height < 0) {
            throw new IllegalArgumentException();
        }

        Note result = new Note();

        for (Vertex v : brain.getBrainGraph().getPropertyGraph().getVertices()) {
            Iterable<Edge> inEdges = v.getEdges(Direction.IN);
            if (!inEdges.iterator().hasNext()) {
                Atom a = brain.getBrainGraph().getAtom(v);
                if (filter.isVisible(v)) {
                    Note n = viewInternal(a, height, filter, style);
                    result.addChild(n);
                }
            }
        }

        Collections.sort(result.getChildren(), new NoteComparator());
        return result;
    }

    public Note findIsolatedAtoms(final Filter filter) {
        if (null == filter) {
            throw new IllegalArgumentException();
        }

        Note result = new Note();

        for (Vertex v : brain.getBrainGraph().getPropertyGraph().getVertices()) {
            if (null != v.getProperty("value")
                    && !v.getEdges(Direction.IN).iterator().hasNext()
                    && !v.getEdges(Direction.OUT).iterator().hasNext()) {
                Atom a = brain.getBrainGraph().getAtom(v);
                if (filter.isVisible(v)) {
                    Note n = viewInternal(a, 1, filter, forwardViewStyle);
                    result.addChild(n);
                }
            }
        }

        return result;
    }

    public void removeIsolatedAtoms(final Filter filter) {
        if (null == filter) {
            throw new IllegalArgumentException();
        }

        List<Vertex> toRemove = new LinkedList<>();

        for (Vertex v : brain.getBrainGraph().getPropertyGraph().getVertices()) {
            if (null != v.getProperty("value")
                    && !v.getEdges(Direction.IN).iterator().hasNext()
                    && !v.getEdges(Direction.OUT).iterator().hasNext()) {
                //Atom a = brain.getBrainGraph().getAtom(v);
                if (filter.isVisible(v)) {
                    toRemove.add(v);
                }
            }
        }

        for (Vertex v : toRemove) {
            // note: we assume from the above that there are no dependent vertices (i.e. list nodes) to remove first
            brain.getBrainGraph().getPropertyGraph().removeVertex(v);
        }

        brain.getBrainGraph().updated();
    }

    /**
     * Performs a Ripple query.
     *
     * @param query  the Ripple query to execute
     * @param height  maximum height of the search results view
     * @param filter a collection of criteria for atoms and links.
     *               Atoms and links which do not meet the criteria are not to appear in search results.
     * @param style  the adjacency style of the view
     * @return an ordered list of query results
     * @throws net.fortytwo.ripple.RippleException
     *          if the query fails in Ripple
     */
    /*     // TODO: restore Ripple support in such a way as to avoid Android/Dalvik issues
    public Note rippleQuery(final String query,
                            final int height,
                            final Filter filter,
                            final AdjacencyStyle style) throws RippleException {
        if (null == query || height < 0 || null == filter || null == style) {
            throw new IllegalArgumentException();
        }

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
            //System.out.println("result list: " + l);
            if (1 == l.length()) {
                Value v = l.getFirst().toRDF(qp.getConnection()).sesameValue();
                if (v instanceof URI && v.stringValue().startsWith(PropertyGraphSail.VERTEX_NS)) {
                    String s = v.stringValue();

                    if (s.startsWith(PropertyGraphSail.VERTEX_NS)) {
                        Vertex vx = graph.getPropertyGraph().getVertex(
                            s.substring(PropertyGraphSail.VERTEX_NS.length()));
                        vertices.add(vx);
                    }
                }
            }
        }

        for (Vertex vx : vertices) {
            Atom a = graph.getAtom(vx);

            if (filter.isVisible(a)) {
                Note n = viewInternal(a, height - 1, filter, style);
                result.addChild(n);
            }
        }

        Collections.sort(result.getChildren(), new NoteComparator());
        return result;
    }
    */

    /**
     * Generates a prioritized list of notes
     *
     * @param filter     a collection of criteria for atoms and links.
     *                   Atoms and links which do not meet the criteria are not to appear in the view.
     * @param maxResults the maximum number of results to return
     * @param priorities the list of priorities to view
     * @return a prioritized list of notes
     */
    public Note priorityView(final Filter filter,
                             final int maxResults,
                             final Priorities priorities) {
        if (null == filter || maxResults < 1 || null == priorities) {
            throw new IllegalArgumentException();
        }

        Note result = new Note();
        result.setValue("priority queue with up to " + maxResults + " results");

        Queue<Atom> queue = priorities.getQueue();
        int i = 0;
        for (Atom a : queue) {
            if (filter.isVisible(a.asVertex())) {
                result.addChild(toNote(a, true));

                if (++i >= maxResults) {
                    break;
                }
            }
        }

        return result;
    }

    private void setProperties(final Atom target,
                               final Note note) {
        String value = note.getValue();

        // Note: "fake" root nodes, as well as no-op or invisible nodes, come with null values.
        // TODO: is this the best way to handle values of "fake" root nodes?
        if (null != value) {
            if (null != brain.getActivityLog()) {
                String prev = target.getValue();
                // Note: assumes value is not null
                if (null == prev || (!prev.equals(value))) {
                    brain.getActivityLog().logUpdate(target);
                }
            }

            target.setValue(value);
            brain.getBrainGraph().indexForSearch(target, value);
        }

        boolean propsSet = false;

        String alias = note.getAlias();

        if (null != alias) {
            if (alias.equals(Note.CLEARME_VALUE)) {
                String prev = target.getAlias();
                if (null != prev) {
                    target.setAlias(null);
                    propsSet = true;
                }
            } else {
                String prev = target.getAlias();
                if (null == prev || !prev.equals(alias)) {
                    target.setAlias(alias);
                    propsSet = true;
                }
            }
        }

        String shortcut = note.getShortcut();

        if (null != shortcut) {
            if (shortcut.equals(Note.CLEARME_VALUE)) {
                String prev = target.getShortcut();
                if (null != prev) {
                    target.setShortcut(null);
                    propsSet = true;
                }
            } else {
                String prev = target.getShortcut();
                if (null == prev || !prev.equals(shortcut)) {
                    target.setShortcut(shortcut);
                    propsSet = true;
                }
            }
        }

        Float priority = note.getPriority();
        if (null != priority) {
            Float p = target.getPriority();
            if (0 == priority) {
                if (null != p) {
                    target.setPriority(null);
                    brain.getPriorities().updatePriority(target);
                    propsSet = true;
                }
            } else {
                if (null == p || (!p.equals(priority))) {
                    target.setPriority(priority);
                    brain.getPriorities().updatePriority(target);
                    propsSet = true;
                }
            }
        }

        Float sharability = note.getSharability();
        if (null != sharability) {
            Float p = target.getSharability();
            if (null == p || (!p.equals(sharability))) {
                target.setSharability(sharability);
                propsSet = true;
            }
        }

        Float weight = note.getWeight();
        if (null != weight) {
            Float p = target.getWeight();
            if (null == p || (!p.equals(weight))) {
                target.setWeight(weight);
                propsSet = true;
            }
        }

        if (propsSet && null != brain.getActivityLog()) {
            brain.getActivityLog().logSetProperties(target);
        }
    }

    private Note toNote(final Atom a,
                        final boolean isVisible) {
        Note n = new Note();

        n.setId((String) a.asVertex().getId());
        n.setWeight(a.getWeight());
        n.setSharability(a.getSharability());
        n.setPriority(a.getPriority());
        n.setCreated(a.getCreated());
        n.setAlias(a.getAlias());
        n.setShortcut(a.getShortcut());

        // The convention for "invisible" notes is to leave the value blank,
        // as well as to avoid displaying any child notes.
        if (isVisible) {
            n.setValue(a.getValue());
        }

        if (null != brain.getKnowledgeBase()) {
            List<KnowledgeBase.AtomClassEntry> entries = brain.getKnowledgeBase().getClassInfo(a);
            if (null != entries && entries.size() > 0) {
                List<String> meta = new LinkedList<>();
                for (KnowledgeBase.AtomClassEntry e : entries) {
                    String ann = "class " + e.getInferredClassName()
                            + " " + e.getScore() + "=" + e.getOutScore() + "+" + e.getInScore();
                    meta.add(ann);
                }

                n.setMeta(meta);
            }
        }

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

    public interface ViewStyle {
        String getName();

        Iterable<Atom> getLinked(Atom root, Filter filter);

        boolean addOnUpdate();

        boolean deleteOnUpdate();
    }

    public static ViewStyle lookupStyle(final String name) {
        for (ViewStyle style : viewStyles) {
            if (name.equals(style.getName())) {
                return style;
            }
        }

        throw new IllegalArgumentException("unknown view style: " + name);
    }

    // TODO: switch to a true linked-list model so that we won't have to create temporary collections for iteration
    // TODO: see also BrainGraph.toList
    public static Iterable<Atom> toIterable(AtomList l) {
        List<Atom> ll = new LinkedList<>();
        while (null != l) {
            ll.add(l.getFirst());
            l = l.getRest();
        }

        return ll;
    }

    public static final ViewStyle forwardViewStyle = new ViewStyle() {
        @Override
        public String getName() {
            return "forward";
        }

        @Override
        public Iterable<Atom> getLinked(final Atom root,
                                        final Filter filter) {
            return toIterable(root.getNotes());
        }

        @Override
        public boolean addOnUpdate() {
            return true;
        }

        @Override
        public boolean deleteOnUpdate() {
            return true;
        }
    };

    public static final ViewStyle forwardAddOnlyViewStyle = new ViewStyle() {
        @Override
        public String getName() {
            return "forward-add-only";
        }

        @Override
        public Iterable<Atom> getLinked(final Atom root,
                                        final Filter filter) {
            return toIterable(root.getNotes());
        }

        @Override
        public boolean addOnUpdate() {
            return true;
        }

        @Override
        public boolean deleteOnUpdate() {
            return false;
        }
    };

    public static final ViewStyle backwardViewStyle = new ViewStyle() {
        @Override
        public String getName() {
            return "backward";
        }

        @Override
        public Iterable<Atom> getLinked(final Atom root,
                                        final Filter filter) {
            List<Atom> results = new LinkedList<>();
            for (AtomList l : root.getFirstOf()) {
                AtomList cur = l;
                AtomList prev = null;
                while (null != cur) {
                    prev = cur;
                    cur = cur.getRestOf();
                }

                Atom a = prev.getNotesOf();
                if (filter.isVisible(a.asVertex())) {
                    results.add(a);
                }
            }

            return results;
        }

        @Override
        public boolean addOnUpdate() {
            return false;
        }

        @Override
        public boolean deleteOnUpdate() {
            return false;
        }
    };

    private static final ViewStyle[] viewStyles = {forwardViewStyle, forwardAddOnlyViewStyle, backwardViewStyle};
}
