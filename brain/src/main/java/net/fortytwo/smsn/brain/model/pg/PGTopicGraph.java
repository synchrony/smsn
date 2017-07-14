package net.fortytwo.smsn.brain.model.pg;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.Node;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
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

    public String idOfAtom(final Atom a) {
        return a.getId();
    }

    // TODO: move me
    public static String iriForId(final String id) {
        return thingNamespace + id;
    }

    public String iriOfAtom(final Atom a) {
        return iriForId(idOfAtom(a));
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
        //return new FilteredAtomGraph(this, filter);
        return copyGraph(filter);
    }

    @Override
    public void notifyOfUpdate() {
        this.lastUpdate = System.currentTimeMillis();
    }

    @Override
    public Optional<Atom> getAtomById(final String id) {
        Vertex v = wrapper.getVertexById(id);

        return null == v ? Optional.empty() : Optional.of(asAtom(v));
    }

    @Override
    public ListNode<Link> createListOfLinks(final Link... elements) {
        return createListOfEntities(SemanticSynchrony.VertexLabels.LIST, this::asListOfLinks, elements);
    }

    @Override
    public ListNode<Topic> createListOfTopics(final Topic... elements) {
        return createListOfEntities(SemanticSynchrony.VertexLabels.LIST, this::asListOfTopics, elements);
    }

    @Override
    public ListNode<TreeNode<Link>> createListOfLinkTrees(
            TreeNode<Link>... elements) {
        return createListOfEntities(SemanticSynchrony.VertexLabels.LIST, this::asListOfLinkTrees, elements);
    }

    @Override
    public ListNode<Atom> createListOfAtoms(final Atom... elements) {
        return createListOfEntities(SemanticSynchrony.VertexLabels.LIST, this::asListOfAtoms, elements);
    }

    @Override
    public Topic createTopic(final String topicId) {
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
    public Atom createAtom(final String id) {
        return createEntity(id, SemanticSynchrony.VertexLabels.ATOM, this::asAtom);
    }

    @Override
    public Atom createAtomWithProperties(final Filter filter,
                                         final String id) {

        Atom atom = createAtom(id);

        atom.setCreated(new Date().getTime());
        atom.setSource(filter.getDefaultSource());
        atom.setWeight(filter.getDefaultWeight());

        return atom;
    }

    public <T extends Node> ListNode<T> createListNode(
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

    public Atom asAtom(final Vertex vertex) {
        Preconditions.checkNotNull(vertex, "vertex");

        return new PGAtom(vertex) {
            @Override
            protected PGTopicGraph getGraph() {
                return PGTopicGraph.this;
            }
        };
    }

    public <T extends Node> ListNode<T> asEntityList(final Vertex vertex, final Function<Vertex, T> constructor) {
        Preconditions.checkNotNull(vertex, "vertex");

        return new PGListNode<T>(vertex, constructor) {
            @Override
            protected PGTopicGraph getGraph() {
                return PGTopicGraph.this;
            }
        };
    }

    public <T extends Node> TreeNode<T> asEntityTree(final Vertex vertex,
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

    public ListNode<Atom> asListOfAtoms(final Vertex vertex) {
        return asEntityList(vertex, this::asAtom);
    }

    public TreeNode<Link> asLinkTree(final Vertex vertex) {
        return asEntityTree(vertex, this::asLink);
    }

    @Override
    public void removeIsolatedAtoms(final Filter filter) {
        Preconditions.checkNotNull(filter, "filter");

        List<Vertex> toRemove = new LinkedList<>();

        propertyGraph.vertices().forEachRemaining(v -> {
            if (isAtomVertex(v)
                    && !v.edges(Direction.IN).hasNext()
                    && !v.edges(Direction.OUT).hasNext()) {
                if (filter.test(asAtom(v))) {
                    toRemove.add(v);
                }
            }
        });

        // note: we assume from the above that there are no dependent vertices (i.e. list nodes) to remove first
        toRemove.forEach(Element::remove);

        notifyOfUpdate();
    }

    @Override
    public void reindexAtom(final Atom atom) {
        updateIndex(atom, SemanticSynchrony.PropertyKeys.ID_V);
        updateIndex(atom, SemanticSynchrony.PropertyKeys.TITLE);
        updateIndex(atom, SemanticSynchrony.PropertyKeys.ACRONYM);
        updateIndex(atom, SemanticSynchrony.PropertyKeys.SHORTCUT);
    }

    void updateIndex(final Atom atom, final String key) {
        wrapper.updateIndex(((PGAtom) atom).asVertex(), key);
    }

    /**
     * @return an Iterable of all atoms in the knowledge base, as opposed to all vertices
     * (many of which are list nodes rather than atoms)
     */
    @Override
    public Iterable<Atom> getAllAtoms() {
        return () -> asFilteredStream(
                getPropertyGraph().vertices(),
                this::isAtomVertex)
                .map(this::asAtom).iterator();
    }

    @Override
    public List<Atom> getAtomsByTitleQuery(final String query, final Filter filter) {
        return filterAndSort(wrapper.getVerticesByTitle(query), filter, query);
    }

    @Override
    public List<Atom> getAtomsByAcronym(final String query, final Filter filter) {
        return filterAndSort(wrapper.getVerticesByAcronym(query.toLowerCase()), filter, query);
    }

    @Override
    public List<Atom> getAtomsByShortcut(final String query, final Filter filter) {
        return filterAndSort(wrapper.getVerticesByShortcut(query), filter, query);
    }

    private <T extends Node> ListNode<T> createListOfEntities(final String vertexLabel,
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

    private boolean isAtomVertex(final Vertex v) {
        String label = v.label();
        return null != label && label.equals(SemanticSynchrony.VertexLabels.ATOM);
    }

    public PGTopicGraph copyGraph(final Filter filter) {
        GraphWrapper newWrapper = new TinkerGraphWrapper(TinkerGraph.open());
        PGTopicGraph newGraph = new PGTopicGraph(newWrapper);

        for (Atom originalAtom : getAllAtoms()) {
            if (filter.test(originalAtom)) {
                PGAtom newAtom = findOrCopyAtom(originalAtom, filter, newGraph);
                PGListNode<Atom> children = (PGListNode<Atom>) originalAtom.getChildren();
                if (null != children) {
                    newAtom.setChildren(copyAtomList(children, filter, newGraph));
                }
            }
        }

        return newGraph;
    }

    private <T> T createEntity(final String id, final String label, final Function<Vertex, T> constructor) {
        Vertex vertex = wrapper.createVertex(id, label);

        return constructor.apply(vertex);
    }

    private List<Atom> filterAndSort(
            final Iterator<Sortable<Vertex, Float>> unranked,
            final Filter filter,
            final String query) {

        List<Sortable<Atom, Float>> ranked = new LinkedList<>();
        while (unranked.hasNext()) {
            Sortable<Vertex, Float> in = unranked.next();
            Atom a = asAtom(in.getEntity());
            if (!filter.test(a)) continue;

            ranked.add(new Sortable<>(a, findScore(in, a, query)));
        }

        Collections.sort(ranked);

        return ranked.stream().map(Sortable::getEntity).collect(Collectors.toList());
    }

    private float findScore(final Sortable<Vertex, Float> in, final Atom a, final String query) {
        float nativeScore = in.getScore();
        float weight = a.getWeight();

        String title = a.getTitle();
        float lengthPenalty = Math.min(1.0f, 1.0f * query.length() / title.length());

        Float priority = a.getPriority();
        float priorityBonus = null == priority ? 1f : 1f + priority;

        return nativeScore * weight * lengthPenalty * priorityBonus;
    }

    private PGAtom findOrCopyAtom(final Atom original, final Filter filter, final TopicGraph newGraph) {
        Optional<Atom> opt = newGraph.getAtomById(original.getId());
        if (opt.isPresent()) return (PGAtom) opt.get();
        PGAtom newAtom = (PGAtom) newGraph.createAtomWithProperties(filter, original.getId());
        newAtom.setSource(original.getSource());

        if (filter.test(original)) {
            newAtom.setTitle(original.getTitle());
            newAtom.setWeight(original.getWeight());
            newAtom.setShortcut(original.getShortcut());
            newAtom.setPriority(original.getPriority());
            newAtom.setAlias(original.getAlias());
            newAtom.setCreated(original.getCreated());
        } else {
            newAtom.setTitle(REDACTED_VALUE);
        }

        return newAtom;
    }

    private ListNode<Atom> copyAtomList(final PGListNode<Atom> original, final Filter filter, final PGTopicGraph newGraph) {
        PGListNode<Atom> originalCur = original;
        PGListNode<Atom> newHead = null, newCur, newPrev = null;
        while (null != originalCur) {
            Atom originalFirst = originalCur.getFirst();
            PGAtom newAtom = findOrCopyAtom(originalFirst, filter, newGraph);
            newCur = (PGListNode<Atom>) newGraph.createListOfAtoms(newAtom);

            if (null == newPrev) {
                newHead = newCur;
            } else {
                newPrev.setRest(newCur);
            }

            newPrev = newCur;
            originalCur = (PGListNode<Atom>) originalCur.getRest();
        }

        return newHead;
    }

    private <A> Stream<A> asFilteredStream(Iterator<A> sourceIterator, Predicate<A> filter) {
        Iterable<A> iterable = () -> sourceIterator;
        Stream<A> stream = StreamSupport.stream(iterable.spliterator(), false);

        return stream.filter(filter);
    }
}
