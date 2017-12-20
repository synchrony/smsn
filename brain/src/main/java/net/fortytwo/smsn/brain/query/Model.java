package net.fortytwo.smsn.brain.query;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.Priorities;
import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.error.InvalidUpdateException;
import net.fortytwo.smsn.brain.io.wiki.WikiFormat;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Property;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.util.ListDiff;
import org.apache.commons.lang.mutable.MutableInt;
import org.parboiled.common.Preconditions;

import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;
import java.util.function.Predicate;
import java.util.logging.Logger;

public class Model {

    protected static final Logger logger = Logger.getLogger(Model.class.getName());

    public enum QueryType {
        FullText, Acronym, Shortcut, Ripple
    }

    private final Brain brain;

    /**
     * @param brain the Extend-o-Brain instance to query and update
     */
    public Model(final Brain brain) {
        Preconditions.checkArgNotNull(brain, "brain");

        this.brain = brain;
    }

    public View view() {
        return new View();
    }

    public Note customView(final Iterable<Note> notes,
                           final Filter filter) {
        checkNoteIterableArg(notes);
        checkFilterArg(filter);

        Note toNote = createNoteDTO();

        Stack<Note> toChildren = new Stack<>();
        for (Note fromChild : notes) {
            if (filter.test(fromChild)) {
                Note toChild = pullInternal(
                        fromChild, 0, filter, ViewStyle.Basic.Forward.getStyle(), true);
                toChildren.add(toChild);
            }
        }
        Note.setChildren(toNote, toChildren);

        return toNote;
    }

    public void update(final Note fromNote,
                       final Note toNote,
                       final int height,
                       final Filter filter,
                       final ViewStyle style) {


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

        Note toNote = createNoteDTO();

        Iterable<Note> results;
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

        Stack<Note> toChildren = new Stack<>();
        for (Note fromChild : results) {
            if (filter.test(fromChild)) {
                Note toChild = pullInternal(fromChild, height - 1, filter, style, true);
                toChildren.add(toChild);
            }
        }
        Note.setChildren(toNote, toChildren);

        toNote.setLabel(queryType.name() + " results for \"" + query + "\"");
        return toNote;
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
    public Note priorityView(final Filter filter,
                             final int maxResults,
                             final Priorities priorities) throws InvalidGraphException {
        checkFilterArg(filter);
        checkPrioritiesArg(priorities);
        checkMaxResultsArg(maxResults);

        Note result = createNoteDTO();
        result.setLabel("priority queue with up to " + maxResults + " results");

        Queue<Note> queue = priorities.getQueue();
        int i = 0;
        for (Note a : queue) {
            if (filter.test(a)) {
                result.addChild(i++, toTreeNode(a, true, true));

                if (i >= maxResults) {
                    break;
                }
            }
        }

        return result;
    }

    private void checkRootArg(final Note root) {
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

    private final Comparator<Note> compareByTopicId = (a, b) -> null == getTopicId(a)
            ? (null == getTopicId(b) ? 0 : -1)
            : (null == getTopicId(b) ? 1 : getTopicId(a).compareTo(getTopicId(b)));

    private final Comparator<Note> compareByProperties = (a, b) -> {
        int cmp = b.getWeight().compareTo(a.getWeight());

        if (0 == cmp) {
            cmp = b.getCreated().compareTo(a.getCreated());
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
        final MutableInt count = new MutableInt(0);
        style.visitLinked(root, filter, note -> count.setValue(1 + count.intValue()));
        return count.intValue();
    }

    private Note pullInternal(final Note root,
                              final int height,
                              final Filter filter,
                              final ViewStyle style,
                              final boolean getProperties) {
        Preconditions.checkNotNull(root);

        NoteDTO toNote = toTreeNode(root, filter.test(root), getProperties);

        if (height > 0) {
            Stack<Note> toChildren = new Stack<>();
            style.visitLinked(root, filter, child -> {
                Note cn = pullInternal(child, height - 1, filter, style, getProperties);
                toChildren.add(cn);
            });
            Note.setChildren(toNote, toChildren);
        }

        // note: some duplicated work
        toNote.setNumberOfChildren(countChildren(root, filter, style));
        toNote.setNumberOfParents(countChildren(root, filter, style.getInverse()));

        return toNote;
    }

    private void pushInternal(final Note fromNote,
                              final Note toNote,
                              final int height,
                              final Filter filter,
                              final ViewStyle style) {

        // updates are pre-ordered w.r.t. setting of properties
        setProperties(fromNote, toNote);

        updateChildren(fromNote, toNote, height, filter, style);
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
        parent.add(indexOfNthVisible(parent.getFirst(), position, filter), child);
    }

    private void removeNoteAt(final Note parent, final int position, final Filter filter) {
        Note removed = (Note) parent.remove(indexOfNthVisible(parent.getFirst(), position, filter));
        removed.destroy();
    }

    private ListDiff.DiffEditor<Note> createEditor(final Note toNote,
                                                   final Filter filter,
                                                   final ViewStyle style,
                                                   final Set<String> childrenCreated,
                                                   final Set<String> childrenAdded) {
        return new ListDiff.DiffEditor<Note>() {
            @Override
            public void add(final int position,
                            final Note fromChild) {
                if (!style.addOnUpdate()) {
                    return;
                }

                Note toChild = createNote(fromChild, filter, childrenCreated);

                addNoteAt(toChild, toChild, position, filter);

                childrenAdded.add(getTopicId(toChild));
            }

            @Override
            public void delete(final int position,
                               final Note toChild) throws InvalidGraphException {
                if (!style.deleteOnUpdate()) {
                    return;
                }

                if (!style.deleteRecursively() && null != toChild.getFirst()) {
                    return;
                }

                removeNoteAt(toNote, position, filter);
            }
        };
    }

    private void updateChildren(final Note fromNote,
                                final Note toNote,
                                final int height,
                                final Filter filter,
                                final ViewStyle style) {

        if (0 >= height || !filter.test(toNote)) {
            return;
        }

        Set<String> childrenCreated = new HashSet<>();
        Set<String> childrenAdded = new HashSet<>();

        ListDiff.DiffEditor<Note> editor = createEditor(toNote, filter, style, childrenCreated, childrenAdded);

        List<Note> before = toJavaList(pullInternal(toNote, 1, filter, style, false).getFirst());
        List<Note> after = toJavaList(fromNote.getFirst());
        List<Note> lcs = ListDiff.longestCommonSubsequence(before, after, compareByTopicId);

        // we are pre-ordered w.r.t. updating lists of children
        ListDiff.applyDiff(before, after, lcs, compareByTopicId, editor);

        Note fromChild = fromNote.getFirst();
        Note toChild = toNote.getFirst();
        while (null != fromChild) {
            // upon adding children:
            // for a child which is a newly created note, also add grandchildren to one level, possibly recursively
            // if a new child is a new note, only update the child, not the grandchildren
            // if a child is not new, update both the child and the grandchildren with decreasing height
            int nextHeight = null == getTopicId(fromNote)
                    ? height - 1
                    : childrenCreated.contains(getTopicId(fromChild))
                    ? 1
                    : childrenAdded.contains(getTopicId(fromChild))
                    ? 0
                    : height - 1;

            // TODO: verify that this can result in multiple log events per call to update()
            pushInternal(fromChild, toChild, nextHeight, filter, style);

            fromChild = (Note) fromChild.getRest();
            toChild = (Note) toChild.getRest();
        }
    }

    // TODO: compute diffs on ListNodes directly, rather than translating them to Java lists
    private <T> List<T> toJavaList(ListNode<T> list) {
        return ListNode.toJavaList(list);
    }

    // retrieve or create an note for the node
    // notes are only created if they appear under a parent which is also an note
    private Note createNote(final Note fromNote,
                            final Filter filter,
                            final Set<String> created) {

        Topic toTopic = getOrCreateTopic(fromNote.getTopic());
        Note toNote = createNote(toTopic, filter);
        created.add(toTopic.getId());

        if (null == getTopicId(fromNote)) {
            setTopicId(fromNote, toTopic.getId());
        }

        return toNote;
    }

    private Topic getOrCreateTopic(final Topic fromTopic) {
        if (null == fromTopic) {
            return createTopic(null);
        }
        Optional<Topic> topic = brain.getTopicGraph().getTopicById(fromTopic.getId());
        return topic.orElseGet(() -> createTopic(fromTopic.getId()));
    }

    private Topic createTopic(final String id) {
        return brain.getTopicGraph().createTopic(id);
    }

    private Note createNote(final Topic topic,
                            final Filter filter) {
        Note newNote = brain.getTopicGraph().createNoteWithProperties(filter, topic);
        newNote.setSource(filter.getDefaultSource());

        return newNote;
    }

    private void setProperties(final Note fromNote,
                               final Note toNote)
            throws InvalidGraphException, InvalidUpdateException {

        for (Property prop : Note.propertiesByKey.values()) {
            Object value = prop.getGetter().apply(fromNote);
            if (value.equals(WikiFormat.CLEARME)) {
                value = null;
            }
            prop.getSetter().accept(toNote, value);
        }
    }

    private static void setProperties(final Note fromNote,
                                      final Note toNote,
                                      final boolean isVisible) {
        for (Property prop : Note.propertiesByKey.values()) {
            // The convention for "invisible" notes is to leave the label and text blank,
            // as well as to avoid displaying any child notes.
            if (isVisible || !isHiddenProperty(prop)) {
                prop.getSetter().accept(toNote, prop.getGetter().apply(fromNote));
            }
        }
    }

    private static <T> boolean isHiddenProperty(final Property<Note, T> prop) {
        String key = prop.getKey();
        return key.equals(SemanticSynchrony.PropertyKeys.LABEL)
                || key.equals(SemanticSynchrony.PropertyKeys.TEXT);
    }

    public static String getTopicId(final Note note) {
        Topic topic = note.getTopic();
        return null == topic ? null : topic.getId();
    }

    public static List<Note> getChildrenAsList(final Note node) {
        return null == node.getFirst() ? new LinkedList<>() : ListNode.toJavaList(node.getFirst());
    }

    public static int countChildren(final Note node) {
        return null == node.getFirst() ? 0 : node.getFirst().length();
    }

    public static void setTopicId(final Note note, final String id) {
        Topic topic = new TopicDTO();
        topic.setId(id);
        note.setTopic(topic);
    }

    private static NoteDTO createNoteDTO() {
        return new NoteDTO();
    }

    private static NoteDTO toTreeNode(final Note note,
                                   final boolean isVisible,
                                   final boolean getProperties) throws InvalidGraphException {
        NoteDTO node = createNoteDTO();

        setTopicId(node, getTopicId(note));

        if (getProperties) {
            setProperties(note, node, isVisible);
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

    private void logView(final Topic topic) {
        if (null != brain.getActivityLog()) {
            brain.getActivityLog().logView(topic);
        }
    }

    private void logUpdate(final Topic topic) {
        if (null != brain.getActivityLog()) {
            brain.getActivityLog().logUpdate(topic);
        }
    }

    public class View {
        private Note root;
        private int height;
        private Filter filter;
        private ViewStyle style;

        public View() {}

        /**
         * Sets the view root
         * @param root the root note of the view
         * @return this view
         */
        public View root(final Note root) {
            this.root = root;
            return this;
        }

        /**
         * Sets the height of the view
         * @param height the maximum height of the view.
         *               A view of height 0 contains only the root,
         *               while a view of height 1 also contains all children of the root,
         *               a view of height 2 all grandchildren, etc.
         * @return this view
         */
        public View height(final int height) {
            this.height = height;
            return this;
        }

        /**
         * Sets the filter of the view
         * @param filter a collection of criteria for notes and links.
         *               Notes and links which do not meet the criteria are not to appear in the view.
         * @return this view
         */
        public View filter(final Filter filter) {
            this.filter = filter;
            return this;
        }

        /**
         * Sets the style of the view
         * @param style the adjacency style of the view
         * @return this view
         */
        public View style(final ViewStyle style) {
            this.style = style;
            return this;
        }

        /**
         * Renders the view to a tree of notes
         * @return a partial view of the graph as a tree
         */
        public Note get() {
            checkRootArg(root);
            checkHeightArg(height, 0);
            checkFilterArg(filter);
            checkStyleArg(style, false);

            Note result = pullInternal(root, height, filter, style, true);
            logView(root.getTopic());
            return result;
        }

        /**
         * Updates the model based on a provided tree
         * @param updatedRoot the tree of the view as it should be seen in a subsequent pull()
         */
        public View put(final Note updatedRoot) {
            checkRootArg(updatedRoot);
            checkHeightArg(height, 0);
            checkFilterArg(filter);
            checkStyleArg(style, true);

            pushInternal(updatedRoot, root, height, filter, style);
            logUpdate(root.getTopic());
            return this;
        }
    }
}
