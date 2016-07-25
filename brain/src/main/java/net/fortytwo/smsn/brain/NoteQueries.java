package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.error.InvalidUpdateException;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomList;
import net.fortytwo.smsn.brain.model.Filter;
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

        Note n = toNote(root, filter.isVisible(root));

        if (height > 0) {
            for (Atom target : style.getLinked(root, filter)) {
                int h = filter.isVisible(target) ? height - 1 : 0;
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
        if (!filter.isVisible(root)) {
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

            if (0 >= height || !filter.isVisible(rootAtom)) {
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

                    childrenAdded.add(atom.getId());

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
            created.add(atom.getId());
        }
        if (null == note.getId()) {
            note.setId(atom.getId());
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

    private boolean isAdjacent(final Atom a, final boolean includeChildren, final boolean includeParents) {
        return (includeChildren && null != a.getNotes())
                || (includeParents && a.getFirstOf().size() > 0);
    }

    private Note findAtoms(final Filter filter,
                           final boolean includeChildren,
                           final boolean includeParents,
                           int height,
                           ViewStyle style) {
        if (null == filter || height < 0) {
            throw new IllegalArgumentException();
        }

        Note result = new Note();

        for (Atom a : brain.getAtomGraph().getAllAtoms()) {
            if (filter.isVisible(a) && !isAdjacent(a, includeChildren, includeParents)) {
                Note n = viewInternal(a, height, filter, style);
                result.addChild(n);
            }
        }

        Collections.sort(result.getChildren(), new NoteComparator());
        return result;
    }

    public Note findRootAtoms(final Filter filter,
                              final ViewStyle style,
                              final int height) {

        boolean includeChildren = style.getDirection().equals(ViewStyle.Direction.Backward);
        boolean includeParents = style.getDirection().equals(ViewStyle.Direction.Forward);

        return findAtoms(filter, includeChildren, includeParents, height, style);
    }

    public Note findIsolatedAtoms(final Filter filter) {
        return findAtoms(filter, true, true, 1, forwardViewStyle);
    }

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
            if (filter.isVisible(a)) {
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

        note.setId(atom.getId());
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
        public enum Direction {Forward, Backward}

        String getName();

        Iterable<Atom> getLinked(Atom root, Filter filter);

        boolean addOnUpdate();

        boolean deleteOnUpdate();

        Direction getDirection();
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

        @Override
        public Direction getDirection() {
            return Direction.Forward;
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

        @Override
        public Direction getDirection() {
            return Direction.Forward;
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
                if (filter.isVisible(a)) {
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

        @Override
        public Direction getDirection() {
            return Direction.Backward;
        }
    };

    private static final ViewStyle[] viewStyles = {forwardViewStyle, forwardAddOnlyViewStyle, backwardViewStyle};
}
