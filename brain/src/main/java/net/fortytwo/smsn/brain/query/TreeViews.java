package net.fortytwo.smsn.brain.query;

import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.Priorities;
import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.error.InvalidUpdateException;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.rdf.KnowledgeBase;
import net.fortytwo.smsn.brain.util.ListDiff;
import org.parboiled.common.Preconditions;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.function.Predicate;
import java.util.logging.Logger;

public class TreeViews {

    protected static final Logger logger = Logger.getLogger(TreeViews.class.getName());

    public enum QueryType {
        FullText, Acronym, Shortcut, Ripple
    }

    private final Brain brain;

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
            if (filter.test(a)) {
                n.addChild(viewInternal(a, 0, filter, ViewStyle.Basic.Forward.getStyle(), true, null));
            }
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

        String rewrittenQuery = rewriteQuery(query);

        Note result = new Note();

        List<Atom> results;
        switch (queryType) {
            case FullText:
                results = brain.getTopicGraph().getAtomsByTitleQuery(rewrittenQuery, filter);
                break;
            case Acronym:
                results = brain.getTopicGraph().getAtomsByAcronym(rewrittenQuery, filter);
                break;
            case Shortcut:
                results = brain.getTopicGraph().getAtomsByShortcut(rewrittenQuery, filter);
                break;
            default:
                throw new IllegalStateException("unexpected query type: " + queryType);
        }

        for (Atom a : results) {
            if (filter.test(a)) {
                Note n = viewInternal(a, height - 1, filter, style, true, null);
                result.addChild(n);
            }
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
        return findAtoms(filter, true, true, 1, ViewStyle.Basic.Forward.getStyle());
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
            if (filter.test(a)) {
                result.addChild(toNote(a, true, true));

                if (++i >= maxResults) {
                    break;
                }
            }
        }

        return result;
    }

    private void checkRootArg(final Atom root) {
        Preconditions.checkArgNotNull(root, "root");
    }

    private void checkRootArg(final Note root) {
        Preconditions.checkArgNotNull(root, "root");
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
        if (!filter.test(root)) {
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
        Preconditions.checkNotNull(root);

        Note note = toNote(root, filter.test(root), getProperties);

        if (height > 0) {
            for (Atom target : style.getLinked(root, filter)) {
                if (filter.test(target)) {
                    addToCache(target, cache);
                    Note cn = viewInternal(target, height - 1, filter, style, getProperties, cache);
                    note.addChild(cn);
                }
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

        Atom rootAtom = getRequiredAtomForNote(rootNote, cache);

        // we are pre-ordered w.r.t. setting of properties
        setProperties(rootAtom, rootNote);

        updateChildren(rootNote, rootAtom, height, filter, style, cache);
    }

    public static <T> int indexOfNthVisible(final EntityList<T> list, final int position, final Predicate<T> filter) {
        EntityList<T> cur = list;

        int index = 0, count = 0;
        while (null != cur) {
            if (filter.test(cur.getFirst())) count++;
            if (count > position) break;
            cur = cur.getRest();
            index++;
        }

        return index;
    }

    private void addAtomAt(final Atom parent, final Atom child, final int position, final Filter filter) {
        parent.addChildAt(child, indexOfNthVisible(parent.getChildren(), position, filter));
    }

    private void removeAtomAt(final Atom parent, final int position, final Filter filter) {
        parent.deleteChildAt(indexOfNthVisible(parent.getChildren(), position, filter));
    }

    private void updateChildren(final Note rootNote,
                                final Atom rootAtom,
                                final int height,
                                final Filter filter,
                                final ViewStyle style,
                                final Map<String, Atom> cache) {

        if (0 >= height || !filter.test(rootAtom)) {
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

                addAtomAt(rootAtom, atom, position, filter);

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

                removeAtomAt(rootAtom, position, filter);

                // log this activity
                if (null != brain.getActivityLog()) {
                    Atom a = getAtomById(note.getId(), cache);
                    if (null != a) {
                        brain.getActivityLog().logUnlink(rootAtom, a);
                    }
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

    // avoids unnecessary (and costly) index lookups by using a cache of already-retrieved atoms
    private Atom getAtomById(final String id, final Map<String, Atom> cache) {
        Atom atom = cache.get(id);
        if (null == atom) {
            Optional<Atom> opt = brain.getTopicGraph().getAtomById(id);
            if (opt.isPresent()) {
                atom = opt.get();
                cache.put(id, atom);
            }
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
        a.setSource(filter.getDefaultSource());

        if (null != brain.getActivityLog()) {
            brain.getActivityLog().logCreate(a);
        }

        return a;
    }

    private boolean isAdjacent(final Atom a, final boolean includeChildren, final boolean includeParents) {
        return (includeChildren && null != a.getChildren())
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
            if (filter.test(a) && !isAdjacent(a, includeChildren, includeParents)) {
                Note n = viewInternal(a, height, filter, style, true, null);
                result.addChild(n);
            }
        }

        Collections.sort(result.getChildren(), compareByProperties);
        return result;
    }

    private void setProperties(final Atom target,
                               final Note note) throws InvalidGraphException, InvalidUpdateException {
        for (Note.Property prop : Note.propertiesByKey.values()) {
            if (prop.isSettable()) {
                Object value = prop.getNoteGetter().apply(note);
                if (null != value) {
                    if (value.equals(Note.CLEARME)) {
                        value = null;
                    }
                    prop.getAtomSetter().accept(target, value);
                }
            }
        }

        if (null != brain.getActivityLog()) {
            brain.getActivityLog().logSetProperties(target);
        }
    }

    private Note toNote(final Atom atom,
                        final boolean isVisible,
                        final boolean getProperties) throws InvalidGraphException {
        Note note = new Note();

        note.setId(atom.getId());

        if (getProperties) {
            note.setWeight(atom.getWeight());
            note.setSource(atom.getSource());
            note.setPriority(atom.getPriority());
            note.setCreated(atom.getCreated());
            note.setAlias(atom.getAlias());
            note.setShortcut(atom.getShortcut());

            // The convention for "invisible" notes is to leave the title and page blank,
            // as well as to avoid displaying any child notes.
            if (isVisible) {
                note.setTitle(atom.getTitle());
                note.setText(atom.getText());
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

    private String rewriteQuery(final String original) {
        String[] parts = original.trim().split("[ \t\n\r]+");
        StringBuilder sb = new StringBuilder();
        String lastPart = null;
        for (String part : parts) {
            if (null == lastPart) {
                sb.append(part);
            } else {
                if (!isSpecialToken(part) && !isSpecialToken(lastPart)) {
                    sb.append(" AND");
                }

                sb.append(" ");
                sb.append(part);
            }

            lastPart = part;
        }

        return sb.toString();
    }

    private boolean isSpecialToken(final String token) {
        return token.equals("AND") || token.equals("OR");
    }

    // TODO: switch to a true linked-list model so that we won't have to create temporary collections for iteration
    // TODO: see also BrainGraph.toList
    public static Iterable<Atom> toFilteredIterable(final EntityList<Atom> list, final Filter filter) {
        EntityList<Atom> cur = list;
        List<Atom> javaList = new LinkedList<>();
        while (null != cur) {
            if (filter.test(cur.getFirst())) {
                javaList.add(cur.getFirst());
            }
            cur = cur.getRest();
        }

        return javaList;
    }

}
