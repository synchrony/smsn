package net.fortytwo.smsn.brain;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.blueprints.Edge;
import com.tinkerpop.blueprints.Vertex;
import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.error.InvalidUpdateException;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomList;
import net.fortytwo.smsn.brain.model.Note;
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

    private final Brain brain;
    //private final QueryEngine rippleQueryEngine;

    /**
     * @param brain the Extend-o-Brain instance to query and update
     */
    public NoteQueries(final Brain brain) {
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
            Atom a = brain.getAtomGraph().getAtom(id);
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
                       final ViewStyle style) {

        if (null == rootNote || height < 0 || null == filter || null == style) {
            throw new IllegalArgumentException();
        }

        if (!style.addOnUpdate() && !style.deleteOnUpdate()) {
            throw new IllegalStateException("can't update in style " + style);
        }

        updateInternal(rootNote, height, filter, style);

        brain.getAtomGraph().notifyOfUpdate();
    }

    private final Comparator<Note> noteComparator = (a, b) -> null == a.getId()
            ? (null == b.getId() ? 0 : -1)
            : (null == b.getId() ? 1 : a.getId().compareTo(b.getId()));

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
                                final ViewStyle style) {

        final Set<String> childrenAdded = new HashSet<>();
        final Set<String> childrenCreated = new HashSet<>();

        final Atom rootAtom = null == rootNote.getId() ? null : brain.getAtomGraph().getAtom(rootNote.getId());
        if (null != rootAtom) {
            setProperties(rootAtom, rootNote);

            if (0 >= height || !filter.isVisible(rootAtom.asVertex())) {
                return;
            }

            ListDiff.DiffEditor<Note> editor = new ListDiff.DiffEditor<Note>() {
                @Override
                public void add(final int position,
                                final Note note) {
                    if (!style.addOnUpdate()) {
                        return;
                    }

                    Atom atom = getAtom(note, filter, childrenCreated);

                    rootAtom.addChildAt(atom, position);

                    childrenAdded.add((String) atom.asVertex().getId());

                    // log this activity
                    if (null != brain.getActivityLog()) {
                        brain.getActivityLog().logLink(rootAtom, atom);
                    }
                }

                @Override
                public void delete(final int position,
                                   final Note note) throws InvalidGraphException {
                    if (!style.deleteOnUpdate()) {
                        return;
                    }

                    rootAtom.deleteChildAt(position);

                    // log this activity
                    if (null != brain.getActivityLog()) {
                        Atom a = brain.getAtomGraph().getAtom(note.getId());
                        brain.getActivityLog().logUnlink(rootAtom, a);
                    }
                }
            };

            List<Note> before = viewInternal(rootAtom, 1, filter, style).getChildren();
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
                    : childrenCreated.contains(n.getId())
                    ? 1
                    : childrenAdded.contains(n.getId())
                    ? 0
                    : height - 1;

            // TODO: verify that this can result in multiple log events per call to update()
            updateInternal(n, h, filter, style);
        }
    }

    private Atom getAtom(final Note note, final Filter filter, final Set<String> created) {
        // retrieve or create an atom for the note
        // atoms are only created if they appear under a parent which is also an atom
        String id = note.getId();
        Atom atom = null == id ? null : brain.getAtomGraph().getAtom(id);

        if (null == atom) {
            atom = createAtom(note.getId(), filter);
            created.add((String) atom.asVertex().getId());
        }
        if (null == note.getId()) {
            note.setId((String) atom.asVertex().getId());
        }

        return atom;
    }

    private Atom createAtom(final String id,
                            final Filter filter) {
        Atom a = brain.getAtomGraph().createAtom(filter, id);

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
                results = brain.getAtomGraph().getAtomsByFulltextQuery(query, filter);
                break;
            case Acronym:
                results = brain.getAtomGraph().getAtomsByAcronymQuery(query, filter);
                break;
            case Shortcut:
                results = brain.getAtomGraph().getAtomsWithShortcut(query, filter);
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

        for (Vertex v : brain.getAtomGraph().getPropertyGraph().getVertices()) {
            Iterable<Edge> inEdges = v.getEdges(Direction.IN);
            if (!inEdges.iterator().hasNext()) {
                Atom a = brain.getAtomGraph().getAtom(v);
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

        for (Vertex v : brain.getAtomGraph().getPropertyGraph().getVertices()) {
            if (null != v.getProperty("value")
                    && !v.getEdges(Direction.IN).iterator().hasNext()
                    && !v.getEdges(Direction.OUT).iterator().hasNext()) {
                Atom a = brain.getAtomGraph().getAtom(v);
                if (filter.isVisible(v)) {
                    Note n = viewInternal(a, 1, filter, forwardViewStyle);
                    result.addChild(n);
                }
            }
        }

        return result;
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
                             final Priorities priorities) throws InvalidGraphException {
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

    private boolean setValue(final Atom target,
                             final String value) {
        // Note: "fake" root nodes, as well as no-op or invisible nodes, come with null values.
        return null != value && target.setValue(value);
    }

    private boolean setAlias(final Atom target,
                             final String alias) {
        if (null != alias) {
            if (alias.equals(Note.CLEARME_VALUE)) {
                return target.setAlias(null);
            } else {
                return target.setAlias(alias);
            }
        } else {
            return false;
        }
    }

    private boolean setShortcut(final Atom target,
                                final String shortcut) {
        if (null != shortcut) {
            if (shortcut.equals(Note.CLEARME_VALUE)) {
                return target.setShortcut(null);
            } else {
                return target.setShortcut(shortcut);
            }
        } else {
            return false;
        }
    }

    private boolean setPriority(final Atom target,
                                 Float priority) {
        if (null != priority) {
            if (0 == priority) priority = null;

            if (target.setPriority(priority)) {
                brain.getPriorities().updatePriority(target);
                return true;
            }
        }

        return false;
    }

    private boolean setSharability(final Atom target,
                                final Float sharability) {
        return null != sharability && target.setSharability(sharability);
    }

    private boolean setWeight(final Atom target,
                                   final Float weight) {
        return null != weight && target.setWeight(weight);
    }

    private void setProperties(final Atom target,
                               final Note note) throws InvalidGraphException, InvalidUpdateException {
        boolean changed = setValue(target, note.getValue())
                | setAlias(target, note.getAlias())
                | setShortcut(target, note.getShortcut())
                | setPriority(target, note.getPriority())
                | setWeight(target, note.getWeight())
                | setSharability(target, note.getSharability());

        if (changed) {
            brain.getAtomGraph().addAtomToIndices(target);

            if (null != brain.getActivityLog()) {
                brain.getActivityLog().logSetProperties(target);
            }
        }
    }

    private Note toNote(final Atom atom,
                        final boolean isVisible) throws InvalidGraphException {
        Note note = new Note();

        note.setId((String) atom.asVertex().getId());
        note.setWeight(atom.getWeight());
        note.setSharability(atom.getSharability());
        note.setPriority(atom.getPriority());
        note.setCreated(atom.getCreated());
        note.setAlias(atom.getAlias());
        note.setShortcut(atom.getShortcut());

        // The convention for "invisible" notes is to leave the value blank,
        // as well as to avoid displaying any child notes.
        if (isVisible) {
            note.setValue(atom.getValue());
        }

        if (null != brain.getKnowledgeBase()) {
            List<KnowledgeBase.AtomClassEntry> entries = brain.getKnowledgeBase().getClassInfo(atom);
            if (null != entries && entries.size() > 0) {
                List<String> meta = new LinkedList<>();
                for (KnowledgeBase.AtomClassEntry e : entries) {
                    String ann = "class " + e.getInferredClassName()
                            + " " + e.getScore() + "=" + e.getOutScore() + "+" + e.getInScore();
                    meta.add(ann);
                }

                note.setMeta(meta);
            }
        }

        return note;
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
            root.forFirstOf(list -> {
                AtomList cur = list;
                AtomList prev = null;
                while (null != cur) {
                    prev = cur;
                    cur = cur.getRestOf();
                }

                Atom a = prev.getNotesOf();
                if (filter.isVisible(a.asVertex())) {
                    results.add(a);
                }
            });

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
