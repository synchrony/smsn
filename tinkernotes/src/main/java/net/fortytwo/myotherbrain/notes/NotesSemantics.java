package net.fortytwo.myotherbrain.notes;

import com.tinkerpop.blueprints.pgm.CloseableSequence;
import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Element;
import com.tinkerpop.blueprints.pgm.Index;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.blueprints.pgm.impls.tg.TinkerGraph;
import com.tinkerpop.frames.FramesManager;
import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.MyOtherBrain;

import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

//import com.tinkerpop.blueprints.pgm.WeightedCloseableSequence;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NotesSemantics {
    private static final String KEYS = "keys";

    private static final int RANDOM_KEY_MAXTRIALS = 100;

    private final IndexableGraph graph;
    private final FramesManager manager;
    private final Index<Vertex> keys;

    public NotesSemantics(final IndexableGraph graph,
                          final FramesManager manager) {
        this.graph = graph;
        this.manager = manager;

        // TODO: it would be more convenient if IndexableGraph would return a null (with getIndex) for a non-existent index, instead of throwing an exception
        boolean indexExists = false;
        for (Index<? extends Element> index : graph.getIndices()) {
            if (index.getIndexName().equals(KEYS)) {
                indexExists = true;
                break;
            }
        }

        if (!indexExists) {
            Set<String> keys = new HashSet<String>();
            keys.add(MyOtherBrain.KEY);
            graph.createAutomaticIndex(KEYS, Vertex.class, keys);
        }

        keys = graph.getIndex(KEYS, Vertex.class);

        // TODO: temporary
        //migrateKeys();
    }

    /**
     * Generates a view of the graph.
     *
     * @param root   the key of the root atom of the view
     * @param depth  the depth of the view.
     *               A view of depth 0 contains only the root,
     *               while a view of depth 1 also contains all children of the root,
     *               a view of depth 2 all grandchildren, etc.
     * @param filter a collection of criteria for atoms and links.
     *               Atoms and links which do not meet the criteria are not to appear in the view.
     * @param style  the style of view to produce
     * @return a partial view of the graph as a tree of <code>Note</code> objects
     */
    public Note view(final Atom root,
                     final int depth,
                     final Filter filter,
                     final ViewStyle style) {
        if (null == root) {
            throw new IllegalArgumentException();
        }

        Atom link, target;

        if (style.isFromTargets()) {
            link = null;
            target = root;
        } else {
            link = root;
            target = null;
        }

        return viewInternal(link, target, depth, filter, style);
    }

    /**
     * Updates the graph.
     *
     * @param root     the root of the subgraph to be updated
     * @param children the children of the root atom
     * @param depth    the minimum depth to which the graph will be updated
     * @param filter   a collection of criteria for atoms and links.
     *                 Atoms and links which do not meet the criteria are not to be affected by the update.
     * @param style    the style of update to push
     * @throws InvalidUpdateException if the update cannot be performed as specified
     */
    public void update(final Atom root,
                       final List<Note> children,
                       final int depth,
                       final Filter filter,
                       final ViewStyle style) throws InvalidUpdateException {
        if (null == root) {
            throw new IllegalArgumentException();
        }

        Atom link, target;

        if (style.isFromTargets()) {
            link = null;
            target = root;
        } else {
            link = root;
            target = null;
        }

        updateInternal(link, target, children, depth, filter, true, style);
    }

    /**
     * Performs full text search.
     *
     * @param query  the search query
     * @param depth  depth of the search results view
     * @param filter a collection of criteria for atoms and links.
     *               Atoms and links which do not meet the criteria are not to appear in search results.
     * @param style  the style of update to push
     * @return an ordered list of query results
     */
    public Note search(final String query,
                       final int depth,
                       final Filter filter,
                       final ViewStyle style) {

        float linkSharability = (filter.minSharability + filter.maxSharability) / 2;

        Note result = new Note();
        result.setTargetValue("query results for \"" + query + "\"");

        // TODO: this relies on a temporary Blueprints hack which only works with Neo4j
        CloseableSequence<Vertex> i = graph.getIndex(Index.VERTICES, Vertex.class).get("value", "%query%" + query);
        try {
            long now = new Date().getTime();
            while (i.hasNext()) {
                Atom a = getAtom(i.next());

                if (filter.isVisible(a)) {

                    float score = // i instanceof WeightedCloseableSequence
                            //? ((WeightedCloseableSequence) i).currentWeight() :
                            1f;
                    //System.err.println("score = " + score + " (" + (i instanceof WeightedCloseableSequence) + ")");

                    score *= a.getWeight();

                    Note n = view(a, depth - 1, filter, style);
                    n.setLinkValue(".");
                    n.setLinkWeight(score);
                    n.setLinkSharability(linkSharability);
                    n.setLinkCreated(now);
                    result.addChild(n);
                }
            }

            Collections.sort(result.getChildren(), new NoteComparator());
            int index = 0;
            for (Note n : result.getChildren()) {
                String k = "" + ++index;
                n.setLinkKey("#######".substring(0, MyOtherBrain.KEY_DIGITS - k.length()) + k);
            }
        } finally {
            i.close();
        }

        return result;
    }

    public Note viewInternal(final Atom rootLink,
                             final Atom rootTarget,
                             final int depth,
                             final Filter filter,
                             final ViewStyle style) {

        Note n = new Note();

        if (null != rootLink) {
            n.setLinkValue(rootLink.getValue());
            n.setLinkKey(rootLink.getKey());
            n.setLinkWeight(rootLink.getWeight());
            n.setLinkSharability(rootLink.getSharability());
            n.setLinkCreated(rootLink.getCreated());
        }

        if (null != rootTarget) {
            n.setTargetValue(rootTarget.getValue());
            n.setTargetKey(rootTarget.getKey());
            n.setTargetWeight(rootTarget.getWeight());
            n.setTargetSharability(rootTarget.getSharability());
            n.setTargetCreated(rootTarget.getCreated());
        }

        if (depth > 0) {
            if (ViewStyle.HYBRID == style || ViewStyle.HYBRID_INVERSE == style) {
                ViewStyle tmpStyle;

                if (null != rootLink) {
                    tmpStyle = style.isInverse() ? ViewStyle.LINKS_INVERSE : ViewStyle.LINKS;
                    for (Atom link : getLinks(rootLink, rootTarget, tmpStyle, filter)) {
                        Atom target = getTarget(link, tmpStyle);

                        if (null == target) {
                            throw new IllegalStateException("link " + link.getKey() + " has no target");
                        }

                        Note cn = viewInternal(link, target, depth - 1, filter, style);
                        cn.setMeta(true);
                        n.addChild(cn);
                    }
                }

                if (null != rootTarget) {
                    tmpStyle = style.isInverse() ? ViewStyle.TARGETS_INVERSE : ViewStyle.TARGETS;
                    for (Atom link : getLinks(rootLink, rootTarget, tmpStyle, filter)) {
                        Atom target = getTarget(link, tmpStyle);

                        if (null == target) {
                            throw new IllegalStateException("link " + link.getKey() + " has no target");
                        }

                        Note cn = viewInternal(link, target, depth - 1, filter, style);
                        n.addChild(cn);
                    }
                }
            } else {
                for (Atom link : getLinks(rootLink, rootTarget, style, filter)) {
                    Atom target = getTarget(link, style);

                    if (null == target) {
                        throw new IllegalStateException("link " + link.getKey() + " has no target");
                    }

                    Note cn = viewInternal(link, target, depth - 1, filter, style);
                    n.addChild(cn);
                }
            }
        }

        return n;
    }

    private void updateInternal(final Atom rootLink,
                                final Atom rootTarget,
                                final List<Note> children,
                                final int depth,
                                final Filter filter,
                                boolean destructive,
                                final ViewStyle style) throws InvalidUpdateException {
        if (depth < 1) {
            destructive = false;
        }

        List<Note> before = viewInternal(rootLink, rootTarget, 1, filter, style).getChildren();

        // Note: link and metalink notes will never collide, as rootLink is never the same as rootTarget and
        // a link can have only one to or from edge.
        Map<String, Note> beforeMap = new HashMap<String, Note>();
        for (Note n : before) {
            beforeMap.put(n.getLinkKey(), n);
        }

        Map<String, Note> afterMap = new HashMap<String, Note>();
        for (Note n : children) {
            if (null != n.getLinkKey()) {
                afterMap.put(n.getLinkKey(), n);
            }
        }

        // Remove any deleted links
        if (destructive) {
            Set<String> alreadyRemoved = new HashSet<String>();
            for (String linkKey : beforeMap.keySet()) {
                if (alreadyRemoved.contains(linkKey)) {
                    continue;
                }
                if (afterMap.keySet().contains(linkKey)) {
                    Note b = beforeMap.get(linkKey);
                    Note a = afterMap.get(linkKey);
                    if (null == a.getTargetKey()) {
                        throw new InvalidUpdateException("non-null link key with null target key");
                    } else if (!a.getTargetKey().equals(b.getTargetKey())) {
                        throw new InvalidUpdateException("target key of updated link has changed");
                    }
                } else {
                    // Avoid attempting to remove a link more than once (if it appears more than once in the tree).
                    alreadyRemoved.add(linkKey);

                    Atom link = getAtom(linkKey);
                    if (null != link) {
                        breakLink(link);
                    }
                }
            }
        }

        // Add any new links, and update fields
        for (Note n : children) {
            Atom target;
            if (null == n.getTargetKey()) {
                target = createAtom(filter);
            } else {
                target = getAtom(n.getTargetKey());
                if (null == target) {
                    throw new InvalidUpdateException("no such atom: " + n.getTargetKey());
                }

                // Note: if we were to equate sharability with security, this would be a security issue,
                // as one could simply guess keys randomly until one finds an actual atom, making it visible.
                // The benefit of sharability is for user interaction, not access control.
                filter.makeVisible(target);
            }
            target.setValue(n.getTargetValue());

            String linkKey = n.getLinkKey();
            boolean createLink = false;

            Atom link = null;

            if (null == linkKey) {
                createLink = true;
                destructive = false;
            } else if (null == beforeMap.get(linkKey)) {
                link = getAtom(linkKey);

                if (linkExists(rootLink, rootTarget, link, target, n.isMeta(), style)) {
                    filter.makeVisible(link);
                    link.setValue(n.getLinkValue());
                } else {
                    createLink = true;
                }

                destructive = false;
            } else {
                Note b = beforeMap.get(linkKey);

                // Validate against the existing link
                if (null == n.getTargetKey()) {
                    throw new InvalidUpdateException("non-null link key '" + linkKey + "' with null target key");
                } else if (!n.getTargetKey().equals(b.getTargetKey())) {
                    throw new InvalidUpdateException("target key of updated link with key '" + linkKey + "' has changed");
                } else if (n.isMeta() != b.isMeta()) {
                    throw new InvalidUpdateException("unknown whether updated link with key '" + linkKey + "' is a metalink: ("
                            + b.isMeta() + " --> " + n.isMeta() + ")");
                }

                link = getAtom(linkKey);
                link.setValue(n.getLinkValue());
            }

            if (createLink) {
                link = createAtom(filter);
                link.setValue(n.getLinkValue());
                setLink(link, rootLink, rootTarget, target, n.isMeta(), style);
            }

            updateInternal(link, target, n.getChildren(), depth - 1, filter, destructive, style);
        }
    }

    private Collection<Atom> getLinks(final Atom link,
                                      final Atom target,
                                      final ViewStyle style,
                                      final Filter filter) {
        switch (style) {
            case TARGETS:
                return getOutlinks(target, filter);
            case LINKS:
                return getOutlinks(link, filter);
            case TARGETS_INVERSE:
                return getInLinks(target, filter);
            case LINKS_INVERSE:
                return getInLinks(link, filter);
            default:
                throw new IllegalStateException("unsupported view style: " + style);
        }
    }

    private Atom getTarget(final Atom link,
                           final ViewStyle style) {
        return style.isInverse() ? link.getFrom() : link.getTo();
    }

    private boolean linkExists(final Atom rootLink,
                               final Atom rootTarget,
                               final Atom link,
                               final Atom target,
                               final boolean meta,
                               final ViewStyle style) {
        if (null == link) {
            return false;
        }

        if (null == target) {
            throw new IllegalArgumentException();
        }

        if (null == link.getFrom() || null == link.getTo()) {
            throw new IllegalStateException("data corruption: link with key '"
                    + link.getKey() + "' is missing a 'to' and/or 'from' edge");
        }

        Atom root = style.isFromLinks() && style.isFromTargets()
                ? (meta ? rootLink : rootTarget)
                : style.isFromLinks()
                ? rootLink
                : rootTarget;

        if (null == root) {
            throw new IllegalArgumentException();
        }

        return style.isInverse()
                ? link.getFrom().getKey().equals(target.getKey()) && link.getTo().getKey().equals(root.getKey())
                : link.getFrom().getKey().equals(root.getKey()) && link.getTo().getKey().equals(target.getKey());
    }

    private void setLink(final Atom link,
                         final Atom rootLink,
                         final Atom rootTarget,
                         final Atom target,
                         final boolean meta,
                         final ViewStyle style) {
        Atom source = style.isFromLinks() && style.isFromTargets()
                ? (meta ? rootLink : rootTarget)
                : style.isFromLinks()
                ? rootLink
                : rootTarget;

        if (style.isInverse()) {
            link.setFrom(target);
            link.setTo(source);
        } else {
            link.setFrom(source);
            link.setTo(target);
        }
    }

    private void breakLink(final Atom link) {
        link.setFrom(null);
        link.setTo(null);
    }

    private Atom getAtom(final Vertex v) {
        return manager.frame(v, Atom.class);
    }

    public Atom getAtom(final String key) {
        if (null == key) {
            throw new IllegalArgumentException("null atom key");
        }

        Vertex v;

        CloseableSequence<Vertex> s = keys.get(MyOtherBrain.KEY, key);
        try {
            if (!s.hasNext()) {
                return null;
            }
            v = s.next();
            if (s.hasNext()) {
                throw new IllegalStateException("multiple vertices with the same key: '" + key + "'");
            }
        } finally {
            s.close();
        }

        return getAtom(v);
    }

    private Atom createAtom(final Filter filter) {
        Atom a = manager.frame(graph.addVertex(null), Atom.class);
        a.setKey(createKey());
        a.setCreated(new Date().getTime());

        // Place sharability and weight in the middle of the applicable ranges.
        a.setSharability((filter.maxSharability + filter.minSharability) / 2);
        a.setWeight((filter.maxWeight + filter.minWeight) / 2);

        return a;
    }

    /*
        For 5-digit numbers of base 64, expect a collision after 32768 trials (on average).
        There are 1,073,741,824 possibilities.

        int base = 64;
        int length = 5;
        BigDecimal poss = new BigDecimal(base).pow(length);
        BigDecimal trials = new BigDecimal(Math.sqrt((double) base)).pow(length);
        System.out.println("For " + length + "-digit numbers of base " + base + ", expect a collision after "
                + trials + " trials (on average).  There are " + poss + " possibilities.");
     */
    private String createKey() {
        for (int j = 0; j < RANDOM_KEY_MAXTRIALS; j++) {
            String key = MyOtherBrain.createRandomKey();
            if (null == getAtom(key)) {
                return key;
            }
        }

        throw new IllegalStateException("no unoccupied keys have been found");
    }

    private Collection<Atom> getInLinks(final Atom source,
                                        final Filter filter) {
        List<Atom> c = new LinkedList<Atom>();

        for (Edge e : source.asVertex().getInEdges(MyOtherBrain.TO)) {
            Atom link = getAtom(e.getOutVertex());
            Atom f = link.getFrom();
            if (null == f) {
                throw new IllegalStateException("vertex " + link.asVertex().getId() + " has a 'to' but no 'from' edge");
            }
            if (filter.isVisible(link) && filter.isVisible(f)) {
                c.add(link);
            }
        }

        Collections.sort(c, new AtomComparator());
        return c;
    }

    private Collection<Atom> getOutlinks(final Atom source,
                                         final Filter filter) {
        List<Atom> c = new LinkedList<Atom>();

        for (Edge e : source.asVertex().getInEdges(MyOtherBrain.FROM)) {
            Atom link = getAtom(e.getOutVertex());
            Atom f = link.getTo();
            if (null == f) {
                throw new IllegalStateException("vertex " + link.asVertex().getId() + " has a 'from' but no 'to' edge");
            }
            if (filter.isVisible(link) && filter.isVisible(f)) {
                c.add(link);
            }
        }

        Collections.sort(c, new AtomComparator());
        return c;
    }

    // Used irregularly
    private void migrateKeys() {
        for (Vertex v : graph.getVertices()) {
            v.setProperty(MyOtherBrain.KEY, createKey());
        }
    }

    private class AtomComparator implements Comparator<Atom> {
        @Override
        public int compare(Atom a, Atom b) {
            int cmp = b.getWeight().compareTo(a.getWeight());

            return 0 == cmp
                    ? b.getCreated().compareTo(a.getCreated())
                    : cmp;
        }
    }

    private class NoteComparator implements Comparator<Note> {
        @Override
        public int compare(Note a, Note b) {
            int cmp = b.getLinkWeight().compareTo(a.getLinkWeight());
            if (0 == cmp) {
                cmp = b.getLinkCreated().compareTo(a.getLinkCreated());

                if (0 == cmp) {
                    cmp = b.getTargetWeight().compareTo(a.getTargetWeight());

                    if (0 == cmp) {
                        cmp = b.getTargetCreated().compareTo(a.getTargetCreated());
                    }
                }
            }

            return cmp;
        }
    }

    public static class InvalidUpdateException extends Exception {
        public InvalidUpdateException(final String message) {
            super(message);
        }
    }

    public enum ViewStyle {
        TARGETS("targets", false, true, false),
        LINKS("links", true, false, false),
        HYBRID("hybrid", true, true, false),
        TARGETS_INVERSE("targets-inverse", false, true, true),
        LINKS_INVERSE("links-inverse", true, false, true),
        HYBRID_INVERSE("hybrid-inverse", true, true, true);

        private final String name;
        private final boolean fromLinks;
        private final boolean fromTargets;
        private final boolean inverse;

        ViewStyle(final String name,
                  final boolean fromLinks,
                  final boolean fromTargets,
                  final boolean inverse) {
            this.name = name;
            this.fromLinks = fromLinks;
            this.fromTargets = fromTargets;
            this.inverse = inverse;
        }

        public String getName() {
            return name;
        }

        public static ViewStyle find(final String name) {
            for (ViewStyle s : values()) {
                if (s.name.equals(name)) {
                    return s;
                }
            }

            return null;
        }

        public boolean isFromLinks() {
            return fromLinks;
        }

        public boolean isFromTargets() {
            return fromTargets;
        }

        public boolean isInverse() {
            return inverse;
        }
    }

    public static void main(final String[] args) throws Exception {

        Filter filter = new Filter(0, 1, 0, 1);

        NotesSyntax p = new NotesSyntax();
        List<Note> notes;

        //InputStream in = new FileInputStream("/tmp/notes.txt");
        InputStream in = new FileInputStream("/Users/josh/notes/notes.txt");
        try {
            notes = p.flatten(p.readContexts(in));
        } finally {
            in.close();
        }

        IndexableGraph graph = new TinkerGraph();
        FramesManager manager = new FramesManager(graph);
        NotesSemantics m = new NotesSemantics(graph, manager);
        Atom root = m.createAtom(filter);
        root.asVertex().setProperty(MyOtherBrain.KEY, "00000");
        root.setValue("Josh's notes");
        m.update(root, notes, 0, filter, ViewStyle.TARGETS);

        //GraphMLWriter.outputGraph(graph, System.out);
        //System.out.println();

        Note n = m.view(root, 3, filter, ViewStyle.TARGETS);
        p.writeNotes(n.getChildren(), System.out);
    }
}
