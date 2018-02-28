package net.fortytwo.smsn.brain.query;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.Priorities;
import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.error.InvalidUpdateException;
import net.fortytwo.smsn.brain.io.wiki.WikiFormat;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Property;
import net.fortytwo.smsn.brain.model.dto.LinkDTO;
import net.fortytwo.smsn.brain.model.dto.ListNodeDTO;
import net.fortytwo.smsn.brain.model.dto.PageDTO;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
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
     * @param root   the root note of the view
     * @param height the maximum height of the view.
     *               A view of height 0 contains only the root,
     *               while a view of height 1 also contains all children of the root,
     *               a view of height 2 all grandchildren, etc.
     * @param filter a collection of criteria for notes and links.
     *               Notes and links which do not meet the criteria are not to appear in the view.
     * @param style  the adjacency style of the view
     * @return a partial view of the graph as a tree of <code>TreeNode</code> objects
     */
    public TreeNode<Link> view(final Note root,
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

    public TreeNode<Link> customView(final Iterable<Note> notes,
                                     final Filter filter) {
        checkNoteIterableArg(notes);
        checkFilterArg(filter);

        TreeNode<Link> n = createTreeNode();

        for (Note a : notes) {
            if (filter.test(a)) {
                n.addChild(viewInternal(a, 0, filter, ViewStyle.Basic.Forward.getStyle(), true, null));
            }
        }

        return n;
    }

    /**
     * Updates the graph.
     *
     * @param root   the root of the TreeNode tree
     * @param height the maximum height of the tree which will be applied to the graph as an update.
     *               If height is 0, only the root node will be affected,
     *               while a height of 1 will also affect children (which have a depth of 1 from the root), etc.
     * @param filter a collection of criteria for notes and links.
     *               Notes and links which do not meet the criteria are not to be affected by the update.
     * @param style  the adjacency style of the view
     * @throws InvalidUpdateException if the update cannot be performed as specified
     */
    public void update(final TreeNode<Link> root,
                       final int height,
                       final Filter filter,
                       final ViewStyle style) {

        checkRootArg(root);
        checkHeightArg(height, 0);
        checkFilterArg(filter);
        checkStyleArg(style, true);

        Map<String, Note> cache = createCache();

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
     * @param filter    a collection of criteria for notes and links.
     *                  Notes and links which do not meet the criteria are not to appear in search results.
     * @param style     the adjacency style of the view
     * @return an ordered list of query results
     */
    public TreeNode<Link> search(final QueryType queryType,
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

        TreeNode<Link> result = createTreeNode();

        List<Note> results;
        switch (queryType) {
            case FullText:
                results = brain.getTopicGraph().getNotesByTitleQuery(rewrittenQuery, filter);
                break;
            case Acronym:
                results = brain.getTopicGraph().getNotesByAcronym(rewrittenQuery, filter);
                break;
            case Shortcut:
                results = brain.getTopicGraph().getNotesByShortcut(rewrittenQuery, filter);
                break;
            default:
                throw new IllegalStateException("unexpected query type: " + queryType);
        }

        results.stream().filter(filter).forEachOrdered(a -> {
            TreeNode<Link> n = viewInternal(a, height - 1, filter, style, true, null);
            result.addChild(n);
        });

        result.getValue().setLabel(queryType.name() + " results for \"" + query + "\"");
        return result;
    }

    public TreeNode<Link> findRootNotes(final Filter filter,
                                        final ViewStyle style,
                                        final int height) {
        checkHeightArg(height, 0);
        checkFilterArg(filter);
        checkStyleArg(style, false);

        boolean includeChildren = style.getDirection().equals(ViewStyle.Direction.Backward);
        boolean includeParents = style.getDirection().equals(ViewStyle.Direction.Forward);

        return findNotes(filter, includeChildren, includeParents, height, style);
    }

    public TreeNode<Link> findIsolatedNotes(final Filter filter) {
        return findNotes(filter, true, true, 1, ViewStyle.Basic.Forward.getStyle());
    }

    /**
     * Generates a prioritized list of notes
     *
     * @param filter     a collection of criteria for nots and links.
     *                   Notes and links which do not meet the criteria are not to appear in the view.
     * @param maxResults the maximum number of results to return
     * @param priorities the list of priorities to view
     * @return a prioritized list of notes
     */
    public TreeNode<Link> priorityView(final Filter filter,
                                       final int maxResults,
                                       final Priorities priorities) throws InvalidGraphException {
        checkFilterArg(filter);
        checkPrioritiesArg(priorities);
        checkMaxResultsArg(maxResults);

        TreeNode<Link> result = createTreeNode();
        result.getValue().setLabel("priority queue with up to " + maxResults + " results");

        Queue<Note> queue = priorities.getQueue();
        int i = 0;
        for (Note a : queue) {
            if (filter.test(a)) {
                result.addChild(toTreeNode(a, true, true));

                if (++i >= maxResults) {
                    break;
                }
            }
        }

        return result;
    }

    private void checkRootArg(final Note root) {
        Preconditions.checkArgNotNull(root, "root");
    }

    private void checkRootArg(final TreeNode<Link> root) {
        Preconditions.checkArgNotNull(root, "root");
    }

    private void checkNoteIterableArg(final Iterable<Note> notes) {
        Preconditions.checkArgNotNull(notes, "notes");
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

    private void addToCache(final Note note, final Map<String, Note> cache) {
        if (null != cache) cache.put(Note.getId(note), note);
    }

    private final Comparator<TreeNode<Link>> compareById = (a, b) -> null == getId(a)
            ? (null == getId(b) ? 0 : -1)
            : (null == getId(b) ? 1 : getId(a).compareTo(getId(b)));

    private final Comparator<TreeNode<Link>> compareByProperties = (a, b) -> {
        int cmp = getWeight(b).compareTo(getWeight(a));

        if (0 == cmp) {
            cmp = getCreated(b).compareTo(getCreated(a));
        }

        return cmp;
    };

    private int countChildren(final Note root,
                              final Filter filter,
                              final ViewStyle style) {
        // If the note is invisible, we can't see whether it has children.
        if (!filter.test(root)) {
            return 0;
        }

        // If the note is visible, we can see its children (although we will not be able to read the titles of any
        // children which are themselves invisible).
        int count = 0;
        for (Note ignored : style.getLinked(root, filter)) count++;
        return count;
    }

    private TreeNode<Link> viewInternal(final Note root,
                                        final int height,
                                        final Filter filter,
                                        final ViewStyle style,
                                        final boolean getProperties,
                                        final Map<String, Note> cache) {
        Preconditions.checkNotNull(root);

        TreeNode<Link> note = toTreeNode(root, filter.test(root), getProperties);

        if (height > 0) {
            for (Note target : style.getLinked(root, filter)) {
                if (filter.test(target)) {
                    addToCache(target, cache);
                    TreeNode<Link> cn = viewInternal(target, height - 1, filter, style, getProperties, cache);
                    note.addChild(cn);
                }
            }
        }

        // note: some duplicated work
        note.setNumberOfChildren(countChildren(root, filter, style));
        note.setNumberOfParents(countChildren(root, filter, style.getInverse()));

        return note;
    }

    private void updateInternal(final TreeNode<Link> rootNode,
                                final int height,
                                final Filter filter,
                                final ViewStyle style,
                                final Map<String, Note> cache) {

        Note rootNote = getRequiredNoteForNode(rootNode, cache);

        // we are pre-ordered w.r.t. setting of properties
        setNoteProperties(rootNode, rootNote);

        updateChildren(rootNode, rootNote, height, filter, style, cache);
    }

    public static <T> int indexOfNthVisible(final ListNode<T> list, final int position, final Predicate<T> filter) {
        ListNode<T> cur = list;

        int index = 0, count = 0;
        while (null != cur) {
            if (filter.test(cur.getFirst())) count++;
            if (count > position) break;
            cur = cur.getRest();
            index++;
        }

        return index;
    }

    private void addNoteAt(final Note parent, final Note child, final int position, final Filter filter) {
        parent.addChildAt(child, indexOfNthVisible(parent.getChildren(), position, filter));
    }

    private void removeNoteAt(final Note parent, final int position, final Filter filter) {
        parent.deleteChildAt(indexOfNthVisible(parent.getChildren(), position, filter));
    }

    private void updateChildren(final TreeNode<Link> rootNode,
                                final Note rootNote,
                                final int height,
                                final Filter filter,
                                final ViewStyle style,
                                final Map<String, Note> cache) {

        if (0 >= height || !filter.test(rootNote)) {
            return;
        }

        Set<String> childrenAdded = new HashSet<>();
        Set<String> childrenCreated = new HashSet<>();

        ListDiff.DiffEditor<TreeNode<Link>> editor = new ListDiff.DiffEditor<TreeNode<Link>>() {
            @Override
            public void add(final int position,
                            final TreeNode<Link> node) {
                if (!style.addOnUpdate()) {
                    return;
                }

                Note note = getNoteForNode(node, filter, childrenCreated, cache);

                addNoteAt(rootNote, note, position, filter);

                childrenAdded.add(Note.getId(note));

                // log this activity
                if (null != brain.getActivityLog()) {
                    brain.getActivityLog().logLink(rootNote, note);
                }
            }

            @Override
            public void delete(final int position,
                               final TreeNode<Link> note) throws InvalidGraphException {
                if (!style.deleteOnUpdate()) {
                    return;
                }

                removeNoteAt(rootNote, position, filter);

                // log this activity
                if (null != brain.getActivityLog()) {
                    Note a = getNoteById(getId(note), cache);
                    if (null != a) {
                        brain.getActivityLog().logUnlink(rootNote, a);
                    }
                }
            }
        };

        List<TreeNode<Link>> before = toJavaList(viewInternal(rootNote, 1, filter, style, false, cache).getChildren());
        List<TreeNode<Link>> after = toJavaList(rootNode.getChildren());
        List<TreeNode<Link>> lcs = ListDiff.longestCommonSubsequence(before, after, compareById);

        // we are pre-ordered w.r.t. updating lists of children
        ListDiff.applyDiff(before, after, lcs, compareById, editor);

        for (TreeNode<Link> n : toJavaList(rootNode.getChildren())) {
            // upon adding children:
            // for a child which is a newly created note, also add grandchildren to one level, possibly recursively
            // if a new child is a new note, only update the child, not the grandchildren
            // if a child is not new, update both the child and the grandchildren with decreasing height
            int h = null == getId(rootNode)
                    ? height - 1
                    : childrenCreated.contains(getId(n))
                    ? 1
                    : childrenAdded.contains(getId(n))
                    ? 0
                    : height - 1;

            // TODO: verify that this can result in multiple log events per call to update()
            updateInternal(n, h, filter, style, cache);
        }
    }

    // TODO: compute diffs on ListNodes directly, rather than translating them to Java lists
    private <T> List<T> toJavaList(ListNode<T> list) {
        return ListNode.toJavaList(list);
    }

    private Map<String, Note> createCache() {
        return new HashMap<>();
    }

    // avoids unnecessary (and costly) index lookups by using a cache of already-retrieved notes
    private Note getNoteById(final String id, final Map<String, Note> cache) {
        Note note = cache.get(id);
        if (null == note) {
            Optional<Note> opt = brain.getTopicGraph().getNoteById(id);
            if (opt.isPresent()) {
                note = opt.get();
                cache.put(id, note);
            }
        }

        return note;
    }

    // retrieve or create an note for the node
    // notes are only created if they appear under a parent which is also an note
    private Note getNoteForNode(final TreeNode<Link> node,
                                final Filter filter,
                                final Set<String> created,
                                final Map<String, Note> cache) {

        String id = getId(node);
        Note note = null == id ? null : getNoteById(id, cache);

        if (null == note) {
            note = createNote(getId(node), filter);
            created.add(Note.getId(note));
            cache.put(Note.getId(note), note);
        }
        if (null == getId(node)) {
            setId(node, Note.getId(note));
        }

        return note;
    }

    private Note getRequiredNoteForNode(final TreeNode<Link> node, final Map<String, Note> cache) {
        if (null == getId(node)) {
            throw new InvalidUpdateException("note has no id");
        }

        Note note = getNoteById(getId(node), cache);
        if (null == note) {
            throw new InvalidUpdateException("no such note: " + getId(node));
        }

        return note;
    }

    private Note createNote(final String id,
                            final Filter filter) {
        Note a = brain.getTopicGraph().createNoteWithProperties(filter, id);
        Note.setSource(a, filter.getDefaultSource());

        if (null != brain.getActivityLog()) {
            brain.getActivityLog().logCreate(a);
        }

        return a;
    }

    private boolean isAdjacent(final Note a, final boolean includeChildren, final boolean includeParents) {
        return (includeChildren && null != a.getChildren())
                || (includeParents && a.getFirstOf().size() > 0);
    }

    private TreeNode<Link> findNotes(final Filter filter,
                                     final boolean includeChildren,
                                     final boolean includeParents,
                                     int height,
                                     ViewStyle style) {
        if (null == filter || height < 0) {
            throw new IllegalArgumentException();
        }

        TreeNode<Link> result = createTreeNode();

        for (Note a : brain.getTopicGraph().getAllNotes()) {
            if (filter.test(a) && !isAdjacent(a, includeChildren, includeParents)) {
                TreeNode<Link> n = viewInternal(a, height, filter, style, true, null);
                result.addChild(n);
            }
        }

        sortChildren(result, compareByProperties);

        return result;
    }

    // TODO: simplify
    private void sortChildren(final TreeNode<Link> node, final Comparator<TreeNode<Link>> comparator) {
        List<TreeNode<Link>> children = ListNode.toJavaList(node.getChildren());
        Collections.sort(children, comparator);
        TreeNode<Link>[] array = (TreeNode<Link>[]) new TreeNode[children.size()];
        children.toArray(array);

        node.setChildren(ListNodeDTO.fromArray(array));
    }

    private void setNoteProperties(final TreeNode<Link> fromNode,
                                   final Note toNote)
            throws InvalidGraphException, InvalidUpdateException {

        for (String key : Note.propertiesByKey.keySet()) {
            setNoteProperty(fromNode, toNote, key);
        }

        if (null != brain.getActivityLog()) {
            brain.getActivityLog().logSetProperties(toNote);
        }
    }

    private <T> void setNoteProperty(
            final TreeNode<Link> fromNode, final Note toNote, final String key) {

        Page page = fromNode.getValue().getPage();
        Property<Note, T> noteProp = (Property<Note, T>) Note.propertiesByKey.get(key);
        Property<Page, T> pageProp = (Property<Page, T>) Page.propertiesByKey.get(key);
        T value = null;
        if (null == pageProp) {
            switch (key) {
                case SemanticSynchrony.PropertyKeys.TITLE:
                    value = (T) fromNode.getValue().getLabel();
                    break;
                case SemanticSynchrony.PropertyKeys.ID:
                    value = (T) fromNode.getValue().getTarget().getId();
                    break;
                default:
                    throw new InvalidUpdateException("no such property: " + key);
            }
        } else {
            if (null != page) {
                value = pageProp.getGetter().apply(page);
            }
        }

        if (null != value) {
            if (value.equals(WikiFormat.CLEARME)) {
                value = null;
            }
            noteProp.getSetter().accept(toNote, value);
        }
    }

    private static void setNodeProperties(final Note from,
                                          final TreeNode<Link> to,
                                          final boolean isVisible) {
        // The convention for "invisible" notes is to leave the title and page blank,
// as well as to avoid displaying any child notes.
        Note.propertiesByKey.keySet().stream().filter(key -> isVisible ||
                (!key.equals(SemanticSynchrony.PropertyKeys.TITLE)
                        && !key.equals(SemanticSynchrony.PropertyKeys.TEXT))).forEach(key -> {
            if (key.equals(SemanticSynchrony.PropertyKeys.TITLE)) {
                to.getValue().setLabel(Note.getTitle(from));
            } else {
                setNodeProperty(from, to, key);
            }
        });
    }

    private static <T> void setNodeProperty(final Note fromNote, final TreeNode<Link> toNode, final String key) {
        // TODO: temporary
        if (!key.equals(SemanticSynchrony.PropertyKeys.ID)) {
            Property<Page, T> pageProp = (Property<Page, T>) Page.propertiesByKey.get(key);
            Property<Note, T> noteProp = (Property<Note, T>) Note.propertiesByKey.get(key);

            pageProp.getSetter().accept(toNode.getValue().getPage(), noteProp.getGetter().apply(fromNote));
        }
    }

    public static String getId(final TreeNode<Link> node) {
        Topic target = node.getValue().getTarget();
        return null == target ? null : target.getId();
    }

    public static Float getWeight(final TreeNode<Link> node) {
        Page page = node.getValue().getPage();
        return null == page ? null : page.getWeight();
    }

    public static Float getPriority(final TreeNode<Link> node) {
        Page page = node.getValue().getPage();
        return null == page ? null : page.getPriority();
    }

    public static String getSource(final TreeNode<Link> node) {
        Page page = node.getValue().getPage();
        return null == page ? null : page.getSource();
    }

    public static Long getCreated(final TreeNode<Link> node) {
        Page page = node.getValue().getPage();
        return null == page ? null : page.getCreated();
    }

    public static String getTitle(final TreeNode<Link> node) {
        return node.getValue().getLabel();
    }

    public static String getText(final TreeNode<Link> node) {
        Page page = node.getValue().getPage();
        return null == page ? null : page.getText();
    }

    public static String getAlias(final TreeNode<Link> node) {
        Page page = node.getValue().getPage();
        return null == page ? null : page.getAlias();
    }

    public static String getShortcut(final TreeNode<Link> node) {
        Page page = node.getValue().getPage();
        return null == page ? null : page.getShortcut();
    }

    public static List<String> getMeta(final TreeNode<Link> node) {
        // TODO
        return new LinkedList<>();
    }

    public static List<TreeNode<Link>> getChildrenAsList(final TreeNode<Link> node) {
        return null == node.getChildren() ? new LinkedList<>() : ListNode.toJavaList(node.getChildren());
    }

    public static int countChildren(final TreeNode<Link> node) {
        return null == node.getChildren() ? 0 : node.getChildren().length();
    }

    public static void setId(final TreeNode<Link> node, final String id) {
        Topic topic = new TopicDTO();
        topic.setId(id);
        node.getValue().setTarget(topic);
    }

    public static void setCreated(final TreeNode<Link> node, final long created) {
        node.getValue().getPage().setCreated(created);
    }

    public static void setAlias(final TreeNode<Link> node, final String alias) {
        node.getValue().getPage().setAlias(alias);
    }

    public static void setShortcut(final TreeNode<Link> node, final String shortcut) {
        node.getValue().getPage().setShortcut(shortcut);
    }

    public static void setTitle(final TreeNode<Link> node, final String title) {
        node.getValue().setLabel(title);
    }

    public static void setText(final TreeNode<Link> node, final String text) {
        node.getValue().getPage().setText(text);
    }

    public static void setSource(final TreeNode<Link> node, final String source) {
        node.getValue().getPage().setSource(source);
    }

    public static void setWeight(final TreeNode<Link> node, final Float weight) {
        node.getValue().getPage().setWeight(weight);
    }

    public static void setPriority(final TreeNode<Link> node, final Float priority) {
        node.getValue().getPage().setPriority(priority);
    }

    private static TreeNode<Link> toTreeNode(final Note note,
                                             final boolean isVisible,
                                             final boolean getProperties) throws InvalidGraphException {
        TreeNode<Link> node = createTreeNode();

        setId(node, Note.getId(note));

        if (getProperties) {
            setNodeProperties(note, node, isVisible);

            /* TODO: restore metadata
            if (null != brain.getKnowledgeBase()) {
                List<KnowledgeBase.NoteClassEntry> entries = brain.getKnowledgeBase().getClassInfo(note);
                if (null != entries && entries.size() > 0) {
                    List<String> meta = new LinkedList<>();
                    for (KnowledgeBase.NoteClassEntry e : entries) {
                        String ann = "class " + e.getInferredClassName()
                                + " " + e.getScore() + "=" + e.getOutScore() + "+" + e.getInScore();
                        meta.add(ann);
                    }

                    node.setMeta(meta);
                }
            }
            */
        }

        return node;
    }

    private String rewriteQuery(final String original) {
        String[] parts = original.trim().split("[ \t\n\r]+");
        StringBuilder sb = new StringBuilder();
        String lastPart = null;
        for (String part : parts) {
            if (null == lastPart) {
                sb.append(part);
            } else {
                if (isNormalToken(part) && isNormalToken(lastPart)) {
                    sb.append(" AND");
                }

                sb.append(" ");
                sb.append(part);
            }

            lastPart = part;
        }

        return sb.toString();
    }

    private boolean isNormalToken(final String token) {
        return !token.equals("AND") && !token.equals("OR");
    }

    private static TreeNode<Link> createTreeNode() {
        TreeNode<Link> node = new TreeNodeDTO<>();
        Link link = new LinkDTO();
        link.setPage(new PageDTO());
        node.setValue(link);
        return node;
    }

    // TODO: switch to a true linked-list model so that we won't have to create temporary collections for iteration
    // TODO: see also BrainGraph.toList
    public static Iterable<Note> toFilteredIterable(final ListNode<Note> list, final Filter filter) {
        ListNode<Note> cur = list;
        List<Note> javaList = new LinkedList<>();
        while (null != cur) {
            if (filter.test(cur.getFirst())) {
                javaList.add(cur.getFirst());
            }
            cur = cur.getRest();
        }

        return javaList;
    }
}
