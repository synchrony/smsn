package net.fortytwo.smsn.brain.model.pg;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Entity;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.model.pg.tg.TinkerGraphWrapper;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;

import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class PGTopicGraph implements TopicGraph {

    private static final String REDACTED_VALUE = "";

    private static final String thingNamespace = SemanticSynchrony.getConfiguration().getThingNamespace();

    private final GraphWrapper wrapper;
    private final Graph propertyGraph;

    private long lastUpdate;

    public PGTopicGraph(final GraphWrapper wrapper) {
        this.wrapper = wrapper;
        this.propertyGraph = wrapper.getGraph();
    }

    public Graph getPropertyGraph() {
        return propertyGraph;
    }

    public AtomId idOf(final Note a) {
        return Note.getId(a);
    }

    // TODO: move me
    public static String iriForId(final AtomId id) {
        return thingNamespace + id.value;
    }

    public String iriOf(final Note a) {
        return iriForId(idOf(a));
    }

    @Override
    public long getLastUpdate() {
        return lastUpdate;
    }

    @Override
    public void begin() {
        wrapper.begin();
    }

    @Override
    public void commit() {
        wrapper.commit();
    }

    @Override
    public void rollback() {
        wrapper.rollback();
    }

    @Override
    public TopicGraph createFilteredGraph(Filter filter) {
        return copyGraph(filter);
    }

    @Override
    public void notifyOfUpdate() {
        this.lastUpdate = System.currentTimeMillis();
    }

    @Override
    public Optional<Note> getNoteById(final AtomId id) {
        Vertex v = wrapper.getVertexById(id);

        return null == v ? Optional.empty() : Optional.of(asNote(v));
    }

    @Override
    public ListNode<Link> toList(final Link... elements) {
        return createListOfEntities(SemanticSynchrony.VertexLabels.LIST, this::asListOfLinks, elements);
    }

    @Override
    public ListNode<Topic> toList(final Topic... elements) {
        return createListOfEntities(SemanticSynchrony.VertexLabels.LIST, this::asListOfTopics, elements);
    }

    @Override
    public ListNode<TreeNode<Link>> toList(
            TreeNode<Link>... elements) {
        return createListOfEntities(SemanticSynchrony.VertexLabels.LIST, this::asListOfLinkTrees, elements);
    }

    @Override
    public ListNode<Note> createListOfNotes(final Note... elements) {
        return createListOfEntities(SemanticSynchrony.VertexLabels.LIST, this::asListOfNotes, elements);
    }

    @Override
    public Topic createTopic(final AtomId topicId) {
        Topic topic = createEntity(null, SemanticSynchrony.VertexLabels.TOPIC, this::asTopic);
        topic.setId(topicId);
        return topic;
    }

    @Override
    public Page createPage(final Link topicLink) {
        Page page = createEntity(null, SemanticSynchrony.VertexLabels.PAGE, this::asPage);
        page.setContent(createTopicTree(topicLink));
        return page;
    }

    @Override
    public Link createLink(final Topic target, final String label, final Role role) {
        Link link = createEntity(null, SemanticSynchrony.VertexLabels.LINK, this::asLink);
        link.setTarget(target);
        link.setLabel(label);
        link.setRole(role);
        return link;
    }

    @Override
    public TreeNode<Link> createTopicTree(final Link link) {
        Preconditions.checkNotNull(link);
        TreeNode<Link> tree
                = createEntity(null, SemanticSynchrony.VertexLabels.TREE, this::asLinkTree);
        tree.setValue(link);
        return tree;
    }

    @Override
    public Note createNote(final AtomId id) {
        return createEntity(id, SemanticSynchrony.VertexLabels.NOTE, this::asNote);
    }

    @Override
    public Note createNoteWithProperties(final Filter filter,
                                         final AtomId id) {

        Note note = createNote(id);

        Note.setCreated(note, new Date().getTime());
        String source = filter.getDefaultSource();
        if (null != source) {
            Note.setSource(note, source);
        }
        Note.setWeight(note, filter.getDefaultWeight());

        return note;
    }

    public <T extends Entity> ListNode<T> createListNode(
            final T first, final ListNode<T> rest, final Function<Vertex, T> constructor) {
        ListNode<T> list = createListOfEntities(SemanticSynchrony.VertexLabels.LIST,
                vertex -> asEntityList(vertex, constructor), first);
        list.setRest(rest);
        return list;
    }

    public Topic asTopic(final Vertex vertex) {
        Preconditions.checkNotNull(vertex, "vertex");

        return new PGTopic(vertex) {
            @Override
            protected PGTopicGraph getGraph() {
                return PGTopicGraph.this;
            }
        };
    }

    public Link asLink(final Vertex vertex) {
        Preconditions.checkNotNull(vertex, "vertex");

        return new PGLink(vertex) {
            @Override
            protected PGTopicGraph getGraph() {
                return PGTopicGraph.this;
            }
        };
    }

    public Page asPage(final Vertex vertex) {
        Preconditions.checkNotNull(vertex, "vertex");

        return new PGPage(vertex) {
            @Override
            protected PGTopicGraph getGraph() {
                return PGTopicGraph.this;
            }
        };
    }

    public Note asNote(final Vertex vertex) {
        Preconditions.checkNotNull(vertex, "vertex");

        return new PGNote(vertex) {
            @Override
            protected PGTopicGraph getGraph() {
                return PGTopicGraph.this;
            }
        };
    }

    public <T extends Entity> ListNode<T> asEntityList(final Vertex vertex, final Function<Vertex, T> constructor) {
        Preconditions.checkNotNull(vertex, "vertex");

        return new PGListNode<T>(vertex, constructor) {
            @Override
            protected PGTopicGraph getGraph() {
                return PGTopicGraph.this;
            }
        };
    }

    public <T extends Entity> TreeNode<T> asEntityTree(final Vertex vertex,
                                                       final Function<Vertex, T> constructor) {
        Preconditions.checkNotNull(vertex, "vertex");

        return new PGTreeNode<T>(vertex, constructor) {
            @Override
            protected PGTopicGraph getGraph() {
                return PGTopicGraph.this;
            }
        };
    }

    public ListNode<Topic> asListOfTopics(final Vertex vertex) {
        return asEntityList(vertex, this::asTopic);
    }

    public ListNode<Link> asListOfLinks(final Vertex vertex) {
        return asEntityList(vertex, this::asLink);
    }

    public ListNode<TreeNode<Link>> asListOfLinkTrees(final Vertex vertex) {
        return asEntityList(vertex, this::asLinkTree);
    }

    public ListNode<Note> asListOfNotes(final Vertex vertex) {
        return asEntityList(vertex, this::asNote);
    }

    public TreeNode<Link> asLinkTree(final Vertex vertex) {
        return asEntityTree(vertex, this::asLink);
    }

    @Override
    public void removeIsolatedNotes(final Filter filter) {
        Preconditions.checkNotNull(filter, "filter");

        List<Vertex> toRemove = new LinkedList<>();

        propertyGraph.vertices().forEachRemaining(v -> {
            if (isNoteVertex(v)
                    && !v.edges(Direction.IN).hasNext()
                    && !v.edges(Direction.OUT).hasNext()) {
                if (filter.test(asNote(v))) {
                    toRemove.add(v);
                }
            }
        });

        // note: we assume from the above that there are no dependent vertices (i.e. list nodes) to remove first
        toRemove.forEach(Element::remove);

        notifyOfUpdate();
    }

    @Override
    public void reindex(final Note note) {
        updateIndex(note, SemanticSynchrony.PropertyKeys.ID);
        updateIndex(note, SemanticSynchrony.PropertyKeys.TITLE);
        updateIndex(note, SemanticSynchrony.PropertyKeys.ACRONYM);
        updateIndex(note, SemanticSynchrony.PropertyKeys.SHORTCUT);
    }

    void updateIndex(final Note note, final String key) {
        wrapper.updateIndex(((PGNote) note).asVertex(), key);
    }

    /**
     * @return an Iterable of all notes in the knowledge base, as opposed to all vertices
     * (many of which are list nodes rather than notes)
     */
    @Override
    public Iterable<Note> getAllNotes() {
        return () -> asFilteredStream(
                getPropertyGraph().vertices(),
                this::isNoteVertex)
                .map(this::asNote).iterator();
    }

    @Override
    public List<Note> getNotesByTitleQuery(final String query, final Filter filter) {
        return filterAndSort(wrapper.getVerticesByTitle(query), filter, query);
    }

    @Override
    public List<Note> getNotesByAcronym(final String query, final Filter filter) {
        return filterAndSort(wrapper.getVerticesByAcronym(query.toLowerCase()), filter, query);
    }

    @Override
    public List<Note> getNotesByShortcut(final String query, final Filter filter) {
        return filterAndSort(wrapper.getVerticesByShortcut(query), filter, query);
    }

    private <T extends Entity> ListNode<T> createListOfEntities(final String vertexLabel,
                                                                final Function<Vertex, ListNode<T>> constructor,
                                                                final T... elements) {
        Preconditions.checkArgument(elements.length > 0);

        ListNode<T> last = null;
        ListNode<T> head = null;
        for (T el : elements) {
            ListNode<T> cur = createEntity(null, vertexLabel, constructor);
            cur.setFirst(el);

            if (null == head) {
                head = cur;
            }
            if (last != null) {
                last.setRest(cur);
            }
            last = cur;
        }

        return head;
    }

    private boolean isNoteVertex(final Vertex v) {
        String label = v.label();
        return null != label && label.equals(SemanticSynchrony.VertexLabels.NOTE);
    }

    public PGTopicGraph copyGraph(final Filter filter) {
        GraphWrapper newWrapper = new TinkerGraphWrapper(TinkerGraph.open());
        PGTopicGraph newGraph = new PGTopicGraph(newWrapper);

        for (Note original : getAllNotes()) {
            if (filter.test(original)) {
                PGNote newNote = findOrCopy(original, filter, newGraph);
                PGListNode<Note> children = (PGListNode<Note>) original.getChildren();
                if (null != children) {
                    newNote.setChildren(copyNoteList(children, filter, newGraph));
                }
            }
        }

        return newGraph;
    }

    private <T> T createEntity(final AtomId id, final String label, final Function<Vertex, T> constructor) {
        Vertex vertex = wrapper.createVertex(id, label);

        return constructor.apply(vertex);
    }

    private List<Note> filterAndSort(
            final Iterator<Sortable<Vertex, Float>> unranked,
            final Filter filter,
            final String query) {

        List<Sortable<Note, Float>> ranked = new LinkedList<>();
        while (unranked.hasNext()) {
            Sortable<Vertex, Float> in = unranked.next();
            Note a = asNote(in.getEntity());
            if (!filter.test(a)) continue;

            ranked.add(new Sortable<>(a, findScore(in, a, query)));
        }

        Collections.sort(ranked);

        return ranked.stream().map(Sortable::getEntity).collect(Collectors.toList());
    }

    private float findScore(final Sortable<Vertex, Float> in, final Note a, final String query) {
        float nativeScore = in.getScore();
        float weight = Note.getWeight(a);

        String title = Note.getTitle(a);
        float lengthPenalty = Math.min(1.0f, 1.0f * query.length() / title.length());

        Float priority = Note.getPriority(a);
        float priorityBonus = null == priority ? 1f : 1f + priority;

        return nativeScore * weight * lengthPenalty * priorityBonus;
    }

    // TODO: copy properties directly
    private PGNote findOrCopy(final Note original, final Filter filter, final TopicGraph newGraph) {
        Optional<Note> opt = newGraph.getNoteById(Note.getId(original));
        if (opt.isPresent()) return (PGNote) opt.get();
        PGNote newNote = (PGNote) newGraph.createNoteWithProperties(filter, Note.getId(original));
        Note.setSource(newNote, Note.getSource(original));

        if (filter.test(original)) {
            Note.setTitle(newNote, Note.getTitle(original));
            Note.setWeight(newNote, Note.getWeight(original));
            Note.setShortcut(newNote, Note.getShortcut(original));
            Note.setPriority(newNote, Note.getPriority(original));
            Note.setAlias(newNote, Note.getAlias(original));
            Note.setCreated(newNote, Note.getCreated(original));
        } else {
            Note.setTitle(newNote, REDACTED_VALUE);
        }

        return newNote;
    }

    private ListNode<Note> copyNoteList(final PGListNode<Note> original, final Filter filter, final PGTopicGraph newGraph) {
        PGListNode<Note> originalCur = original;
        PGListNode<Note> newHead = null, newCur, newPrev = null;
        while (null != originalCur) {
            Note originalFirst = originalCur.getFirst();
            PGNote newNote = findOrCopy(originalFirst, filter, newGraph);
            newCur = (PGListNode<Note>) newGraph.createListOfNotes(newNote);

            if (null == newPrev) {
                newHead = newCur;
            } else {
                newPrev.setRest(newCur);
            }

            newPrev = newCur;
            originalCur = (PGListNode<Note>) originalCur.getRest();
        }

        return newHead;
    }

    private <A> Stream<A> asFilteredStream(Iterator<A> sourceIterator, Predicate<A> filter) {
        Iterable<A> iterable = () -> sourceIterator;
        Stream<A> stream = StreamSupport.stream(iterable.spliterator(), false);

        return stream.filter(filter);
    }
}
