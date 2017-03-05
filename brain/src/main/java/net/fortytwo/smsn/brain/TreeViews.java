package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.error.InvalidUpdateException;
import net.fortytwo.smsn.brain.io.markdown.MarkdownParser;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.rdf.KnowledgeBase;
import net.fortytwo.smsn.brain.util.ListDiff;
import org.parboiled.common.Preconditions;

import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.logging.Logger;

public class TreeViews {

    protected static final Logger logger = Logger.getLogger(TreeViews.class.getName());

    public enum QueryType {
        FullText, Acronym, Shortcut, Ripple
    }

    private final Brain brain;
    //private final QueryEngine rippleQueryEngine;
    private final MarkdownParser markdownParser = new MarkdownParser();

    /**
     * @param brain the Extend-o-Brain instance to query and update
     */
    public TreeViews(final Brain brain) {
        Preconditions.checkArgNotNull(brain, "brain");

        this.brain = brain;
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
        checkRootArg(root);
        checkHeightArg(height, 0);
        checkFilterArg(filter);
        checkStyleArg(style, false);

        if (null != brain.getActivityLog()) {
            brain.getActivityLog().logView(root);
        }

        return viewInternal(root, height, filter, style, true, null);
    }

    public Note customView(final Iterable<Atom> atoms,
                           final Filter filter) {
        checkAtomIterableArg(atoms);
        checkFilterArg(filter);

        Note n = new Note();

        for (Atom a : atoms) {
            n.addChild(viewInternal(a, 0, filter, forwardViewStyle, true, null));
        }

        return n;
    }

    /**
     * Updates the graph.
     *
     * @param root   the root of the note tree
     * @param height the maximum height of the tree which will be applied to the graph as an update.
     *               If height is 0, only the root node will be affected,
     *               while a height of 1 will also affect children (which have a depth of 1 from the root), etc.
     * @param filter a collection of criteria for atoms and links.
     *               Atoms and links which do not meet the criteria are not to be affected by the update.
     * @param style  the adjacency style of the view
     * @throws InvalidUpdateException if the update cannot be performed as specified
     */
    public void update(final Note root,
                       final int height,
                       final Filter filter,
                       final ViewStyle style) {

        checkRootArg(root);
        checkHeightArg(height, 0);
        checkFilterArg(filter);
        checkStyleArg(style, true);

        Map<String, Atom> cache = createCache();

        updateInternal(root, height, filter, style, cache);

        brain.getTopicGraph().notifyOfUpdate();
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
        checkQueryTypeArg(queryType);
        checkQueryArg(query);
        checkHeightArg(height, 1);
        checkFilterArg(filter);
        checkStyleArg(style, false);

        Note result = new Note();

        List<Atom> results;
        switch (queryType) {
            case FullText:
                results = brain.getTopicGraph().getAtomsByTitleQuery(query, filter);
                break;
            case Acronym:
                results = brain.getTopicGraph().getAtomsByAcronym(query, filter);
                break;
            case Shortcut:
                results = brain.getTopicGraph().getAtomsByShortcut(query, filter);
                break;
            default:
                throw new IllegalStateException("unexpected query type: " + queryType);
        }

        for (Atom a : results) {
            Note n = viewInternal(a, height - 1, filter, style, true, null);
            result.addChild(n);
        }

        result.setTitle(queryType.name() + " results for \"" + query + "\"");
        return result;
    }

    public Note findRootAtoms(final Filter filter,
                              final ViewStyle style,
                              final int height) {
        checkHeightArg(height, 0);
        checkFilterArg(filter);
        checkStyleArg(style, false);

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
        checkFilterArg(filter);
        checkPrioritiesArg(priorities);
        checkMaxResultsArg(maxResults);

        Note result = new Note();
        result.setTitle("priority queue with up to " + maxResults + " results");

        Queue<Atom> queue = priorities.getQueue();
        int i = 0;
        for (Atom a : queue) {
            if (filter.isVisible(a)) {
                result.addChild(toNote(a, true, true));

                if (++i >= maxResults) {
                    break;
                }
            }
        }

        return result;
    }

    public void updatePage(final Atom atom,
                           final String page) throws IOException {
        Note pageRoot = markdownParser.parse(page);


    }

    private void deleteMarkdownTree(final Atom atom) {

    }

    private void checkRootArg(final Atom root) {
        Preconditions.checkArgNotNull(root, "root");
    }

    private void checkRootArg(final Note root) {
        Preconditions.checkArgNotNull(root, "root");
    }

    private void checkListOfIdsArg(final List<String> atomIds) {
        Preconditions.checkArgNotNull(atomIds, "atomIds");
    }

    private void checkAtomIterableArg(final Iterable<Atom> atoms) {
        Preconditions.checkArgNotNull(atoms, "atoms");
    }

    private void checkFilterArg(final Filter filter) {
        Preconditions.checkArgNotNull(filter, "filter");
    }

    private void checkStyleArg(final ViewStyle style, boolean isUpdate) {
        Preconditions.checkArgNotNull(style, "style");

        Preconditions.checkArgument(!isUpdate || style.addOnUpdate() || style.deleteOnUpdate(),
                "can't update in style " + style);
    }

    private void checkHeightArg(final int height, final int min) {
        Preconditions.checkArgument(height >= min, "height of " + height + "expecting >= " + min);
    }

    private void checkMaxResultsArg(final int maxResults) {
        Preconditions.checkArgument(maxResults >= 1, "invalid maxResults");
    }

    private void checkQueryTypeArg(final QueryType type) {
        Preconditions.checkArgNotNull(type, "queryType");
    }

    private void checkQueryArg(final String query) {
        Preconditions.checkArgNotNull(query, "query");
    }

    private void checkPrioritiesArg(final Priorities priorities) {
        Preconditions.checkArgNotNull(priorities, "priorities");
    }

    private void addToCache(final Atom atom, final Map<String, Atom> cache) {
        if (null != cache) cache.put(atom.getId(), atom);
    }

    private final Comparator<Note> compareById = (a, b) -> null == a.getId()
            ? (null == b.getId() ? 0 : -1)
            : (null == b.getId() ? 1 : a.getId().compareTo(b.getId()));

    private final Comparator<Note> compareByProperties = (a, b) -> {
        int cmp = b.getWeight().compareTo(a.getWeight());

        if (0 == cmp) {
            cmp = b.getCreated().compareTo(a.getCreated());
        }

        return cmp;
    };

    private int countChildren(final Atom root,
                              final Filter filter,
                              final ViewStyle style) {
        // If the note is invisible, we can't see whether it has children.
        if (!filter.isVisible(root)) {
            return 0;
        }

        // If the note is visible, we can see its children (although we will not be able to read the titles of any
        // children which are themselves invisible).
        int count = 0;
        for (Atom ignored : style.getLinked(root, filter)) count++;
        return count;
    }

    private Note viewInternal(final Atom root,
                              final int height,
                              final Filter filter,
                              final ViewStyle style,
                              final boolean getProperties,
                              final Map<String, Atom> cache) {
        if (null == root) {
            throw new IllegalStateException("null view root");
        }

        Note note = toNote(root, filter.isVisible(root), getProperties);

        if (height > 0) {
            for (Atom target : style.getLinked(root, filter)) {
                addToCache(target, cache);
                int h = filter.isVisible(target) ? height - 1 : 0;
                Note cn = viewInternal(target, h, filter, style, getProperties, cache);
                note.addChild(cn);
            }
        }

        // note: some duplicated work
        note.setNumberOfChildren(countChildren(root, filter, style));
        note.setNumberOfParents(countChildren(root, filter, style.getInverse()));

        return note;
    }

    private void updateInternal(final Note rootNote,
                                final int height,
                                final Filter filter,
                                final ViewStyle style,
                                final Map<String, Atom> cache) {
        validateNote(rootNote);

        Atom rootAtom = getRequiredAtomForNote(rootNote, cache);

        // we are pre-ordered w.r.t. setting of properties
        setProperties(rootAtom, rootNote);

        updateChildren(rootNote, rootAtom, height, filter, style, cache);
    }

    private void validateNote(final Note note) {
        if (null != note.getPage() && 0 < note.getChildren().size()) {
            throw new InvalidUpdateException("atom with page cannot have children");
        }
    }

    private void updateChildren(final Note rootNote,
                                final Atom rootAtom,
                                final int height,
                                final Filter filter,
                                final ViewStyle style,
                                final Map<String, Atom> cache) {

        if (0 >= height || !filter.isVisible(rootAtom)) {
            return;
        }

        Set<String> childrenAdded = new HashSet<>();
        Set<String> childrenCreated = new HashSet<>();

        ListDiff.DiffEditor<Note> editor = new ListDiff.DiffEditor<Note>() {
            @Override
            public void add(final int position,
                            final Note note) {
                if (!style.addOnUpdate()) {
                    return;
                }

                Atom atom = getAtomForNote(note, filter, childrenCreated, cache);

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
                    Atom a = getAtomById(note.getId(), cache);
                    brain.getActivityLog().logUnlink(rootAtom, a);
                }
            }
        };

        List<Note> before = viewInternal(rootAtom, 1, filter, style, false, cache).getChildren();
        List<Note> after = rootNote.getChildren();
        List<Note> lcs = ListDiff.longestCommonSubsequence(before, after, compareById);

        // we are pre-ordered w.r.t. updating lists of children
        ListDiff.applyDiff(before, after, lcs, compareById, editor);

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
            updateInternal(n, h, filter, style, cache);
        }
    }

    private Map<String, Atom> createCache() {
        return new HashMap<>();
    }

    private Atom getAtomById(final String id) {
        return brain.getTopicGraph().getAtomById(id);
    }

    // avoids unnecessary (and costly) index lookups by using a cache of already-retrieved atoms
    private Atom getAtomById(final String id, final Map<String, Atom> cache) {
        Atom atom = cache.get(id);
        if (null == atom) {
            atom = brain.getTopicGraph().getAtomById(id);
            if (null != atom) cache.put(id, atom);
        }
        return atom;
    }

    // retrieve or create an atom for the note
    // atoms are only created if they appear under a parent which is also an atom
    private Atom getAtomForNote(final Note note,
                                final Filter filter,
                                final Set<String> created,
                                final Map<String, Atom> cache) {

        String id = note.getId();
        Atom atom = null == id ? null : getAtomById(id, cache);

        if (null == atom) {
            atom = createAtom(note.getId(), filter);
            created.add(atom.getId());
            cache.put(atom.getId(), atom);
        }
        if (null == note.getId()) {
            note.setId(atom.getId());
        }

        return atom;
    }

    private Atom getRequiredAtomForNote(final Note note, final Map<String, Atom> cache) {
        if (null == note.getId()) {
            throw new InvalidUpdateException("note has no id");
        }

        Atom atom = getAtomById(note.getId(), cache);
        if (null == atom) {
            throw new InvalidUpdateException("no such atom: " + note.getId());
        }

        return atom;
    }

    private Atom createAtom(final String id,
                            final Filter filter) {
        Atom a = brain.getTopicGraph().createAtomWithProperties(filter, id);

        if (null != brain.getActivityLog()) {
            brain.getActivityLog().logCreate(a);
        }

        return a;
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

        for (Atom a : brain.getTopicGraph().getAllAtoms()) {
            if (filter.isVisible(a) && !isAdjacent(a, includeChildren, includeParents)) {
                Note n = viewInternal(a, height, filter, style, true, null);
                result.addChild(n);
            }
        }

        Collections.sort(result.getChildren(), compareByProperties);
        return result;
    }

    private boolean setTitle(final Atom target,
                             final String title) {
        // Note: "fake" root nodes, as well as no-op or invisible nodes, come with null titles.
        return null != title && target.setTitle(title);
    }

    private boolean setPage(final Atom target,
                            final String page) {
        // Note: can't delete page with a view update
        return null != page && target.setText(page);
    }

    private boolean setAlias(final Atom target,
                             final String alias) {
        if (null != alias) {
            if (alias.equals(Note.CLEARME)) {
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
            if (shortcut.equals(Note.CLEARME)) {
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
        boolean changed = setTitle(target, note.getTitle())
                | setPage(target, note.getPage())
                | setAlias(target, note.getAlias())
                | setShortcut(target, note.getShortcut())
                | setPriority(target, note.getPriority())
                | setWeight(target, note.getWeight())
                | setSharability(target, note.getSharability());

        if (changed) {
            brain.getTopicGraph().reindexAtom(target);

            if (null != brain.getActivityLog()) {
                brain.getActivityLog().logSetProperties(target);
            }
        }
    }

    private Note toNote(final Atom atom,
                        final boolean isVisible,
                        final boolean getProperties) throws InvalidGraphException {
        Note note = new Note();

        note.setId(atom.getId());

        if (getProperties) {
            note.setWeight(atom.getWeight());
            note.setSharability(atom.getSharability());
            note.setPriority(atom.getPriority());
            note.setCreated(atom.getCreated());
            note.setAlias(atom.getAlias());
            note.setShortcut(atom.getShortcut());

            // The convention for "invisible" notes is to leave the title and page blank,
            // as well as to avoid displaying any child notes.
            if (isVisible) {
                note.setTitle(atom.getTitle());
                note.setPage(atom.getText());
            }

            if (null != brain.getKnowledgeBase()) {
                List<KnowledgeBase.AtomClassEntry> entries = brain.getKnowledgeBase().getClassInfo(atom);
                if (null != entries && entries.size() > 0) {
                    List<String> meta = new java.util.LinkedList();
                    for (KnowledgeBase.AtomClassEntry e : entries) {
                        String ann = "class " + e.getInferredClassName()
                                + " " + e.getScore() + "=" + e.getOutScore() + "+" + e.getInScore();
                        meta.add(ann);
                    }

                    note.setMeta(meta);
                }
            }
        }

        return note;
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
    public static Iterable<Atom> toIterable(EntityList<Atom> l) {
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

        @Override
        public ViewStyle getInverse() {
            return backwardViewStyle;
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

        @Override
        public ViewStyle getInverse() {
            return backwardViewStyle;
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
                EntityList<Atom> cur = list;
                EntityList<Atom> prev = null;
                while (null != cur) {
                    prev = cur;
                    cur = cur.getRestOf();
                }

                Atom a = prev.getFirst().getSubject(prev);
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

        @Override
        public ViewStyle getInverse() {
            return forwardViewStyle;
        }
    };

    private static final ViewStyle[] viewStyles = {forwardViewStyle, forwardAddOnlyViewStyle, backwardViewStyle};
}
