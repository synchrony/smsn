package net.fortytwo.smsn.brain.model.pg;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Entity;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.model.pg.tg.TinkerGraphWrapper;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;

import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Stack;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class PGTopicGraph implements TopicGraph {

    private static final String REDACTED_VALUE = "";

    private final GraphWrapper wrapper;
    private final Graph propertyGraph;

    public PGTopicGraph(final GraphWrapper wrapper) {
        this.wrapper = wrapper;
        this.propertyGraph = wrapper.getGraph();
    }

    public Graph getPropertyGraph() {
        return propertyGraph;
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
    public Optional<Topic> getTopicById(final String id) {
        Vertex v = wrapper.getVertexById(id);

        return null == v ? Optional.empty() : Optional.of(asTopic(v));
    }

    @Override
    public String idOf(Topic topic) {
        return topic.getId();
    }

    @Override
    public String iriOf(Topic topic) {
        return SemanticSynchrony.iriForId(topic.getId());
    }

    @Override
    public Topic createTopic(final String topicId) {
        Topic topic = createEntity(null, SemanticSynchrony.VertexLabels.TOPIC, this::asTopic);
        topic.setId(topicId);
        return topic;
    }

    @Override
    public Note createNote(final Topic topic, final String label, final Role role) {
        Note note = createNote(topic);
        note.setLabel(label);
        note.setRole(role);
        return note;
    }

    private Note createNote(final Topic topic) {
        Note note = createEntity(null, SemanticSynchrony.VertexLabels.NOTE, this::asNote);
        note.setTopic(topic);
        return note;
    }

    @Override
    public Note createNoteWithProperties(final Filter filter,
                                         final Topic topic) {

        Note note = createNote(topic);

        note.setCreated(new Date().getTime());
        note.setSource(filter.getDefaultSource());
        note.setWeight(filter.getDefaultWeight());

        return note;
    }

    public <T extends Entity> ListNode<T> createListNode(
            final T first, final ListNode<T> rest, final Function<Vertex, T> constructor, final String label) {
        ListNode<T> list = createListOfEntities(label,
                vertex -> asEntityList(vertex, constructor, label), first);
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

    public Note asNote(final Vertex vertex) {
        Preconditions.checkNotNull(vertex, "vertex");

        return new PGNote(vertex, this::asNote) {
            @Override
            protected PGTopicGraph getGraph() {
                return PGTopicGraph.this;
            }
        };
    }

    public <T extends Entity> ListNode<T> asEntityList(
            final Vertex vertex, final Function<Vertex, T> constructor, final String label) {
        Preconditions.checkNotNull(vertex, "vertex");

        return new PGListNode<T>(vertex, constructor, label) {
            @Override
            protected PGTopicGraph getGraph() {
                return PGTopicGraph.this;
            }
        };
    }

    @Override
    public void reindex(final Note note) {
        updateIndex(note, SemanticSynchrony.PropertyKeys.ID);
        updateIndex(note, SemanticSynchrony.PropertyKeys.LABEL);
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

    private void forAllRootNotes(final Filter filter, final Consumer<Note> consumer) {
        for (Note note : getAllNotes()) {
            if (filter.test(note) && Note.isRoot(note)) {
                consumer.accept(note);
            }
        }
    }

    public PGTopicGraph copyGraph(final Filter filter) {
        GraphWrapper newWrapper = new TinkerGraphWrapper(TinkerGraph.open());
        PGTopicGraph newGraph = new PGTopicGraph(newWrapper);

        forAllRootNotes(filter, oldNote -> copyTree(oldNote, filter, newGraph));

        return newGraph;
    }

    private Note copyTree(final Note oldNote, final Filter filter, final TopicGraph newGraph) {
        PGNote newNote = copyNote(oldNote, filter, newGraph);
        Note child = oldNote.getFirst();
        if (null != child) {
            Stack<Note> children = new Stack<>();
            while (null != child) {
                children.push(copyTree(child, filter, newGraph));
                child = (Note) child.getRest();
            }
            Note.setChildren(newNote, children);
        }
        return newNote;
    }

    private <T> T createEntity(final String id, final String label, final Function<Vertex, T> constructor) {
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
        float weight = a.getWeight();

        String title = a.getLabel();
        float lengthPenalty = Math.min(1.0f, 1.0f * query.length() / title.length());

        Float priority = a.getPriority();
        float priorityBonus = null == priority ? 1f : 1f + priority;

        return nativeScore * weight * lengthPenalty * priorityBonus;
    }

    private PGNote copyNote(final Note oldNote, final Filter filter, final TopicGraph newGraph) {
        Topic oldTopic = oldNote.getTopic();
        Topic newTopic = newGraph.createTopic(oldTopic.getId());
        PGNote newNote = (PGNote) newGraph.createNoteWithProperties(filter, newTopic);

        if (filter.test(oldNote)) {
            Note.copyProperties(newNote, oldNote);
        } else {
            newNote.setSource(oldNote.getSource());
            newNote.setLabel(REDACTED_VALUE);
        }

        return newNote;
    }

    private <A> Stream<A> asFilteredStream(Iterator<A> sourceIterator, Predicate<A> filter) {
        Iterable<A> iterable = () -> sourceIterator;
        Stream<A> stream = StreamSupport.stream(iterable.spliterator(), false);

        return stream.filter(filter);
    }
}
