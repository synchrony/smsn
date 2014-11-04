package net.fortytwo.extendo.brain.rdf;

import info.aduna.iteration.CloseableIteration;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.AtomList;
import net.fortytwo.extendo.brain.BrainGraph;
import net.fortytwo.extendo.brain.rdf.classes.AKAReference;
import net.fortytwo.extendo.brain.rdf.classes.BibtexReference;
import net.fortytwo.extendo.brain.rdf.classes.Date;
import net.fortytwo.extendo.brain.rdf.classes.DatedEvent;
import net.fortytwo.extendo.brain.rdf.classes.Document;
import net.fortytwo.extendo.brain.rdf.classes.ISBNReference;
import net.fortytwo.extendo.brain.rdf.classes.LinkedConcept;
import net.fortytwo.extendo.brain.rdf.classes.Person;
import net.fortytwo.extendo.brain.rdf.classes.QuotedValue;
import net.fortytwo.extendo.brain.rdf.classes.RFIDReference;
import net.fortytwo.extendo.brain.rdf.classes.TODOTask;
import net.fortytwo.extendo.brain.rdf.classes.URLReference;
import net.fortytwo.extendo.brain.rdf.classes.WebPage;
import net.fortytwo.extendo.brain.rdf.classes.collections.DocumentCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.EventCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.GenericCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.InterestCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.NoteCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.PersonCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.QuotedValueCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.TODOCollection;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.Rio;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;
import org.openrdf.sail.memory.MemoryStore;

import java.io.OutputStream;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
/*
typical steps in the mapping process:
1) identify simple types
2) identify compound types
    "strict" matching is used to find independent instances of types
    non-strict matching is used for fields of potential independent instances
3) return to (2) until no more compound types are found
4) coerce not-yet-typed container members
5) map to RDF
 */
public class KnowledgeBase {
    private static final Logger logger = Extendo.getLogger(KnowledgeBase.class);

    private final BrainGraph graph;

    private final Map<Class<? extends AtomClass>, AtomClass> classes;

    private final Map<Atom, List<AtomClassEntry>> atomClassifications;

    public KnowledgeBase(final BrainGraph graph) {
        this.graph = graph;
        this.atomClassifications = new HashMap<Atom, List<AtomClassEntry>>();
        this.classes = new HashMap<Class<? extends AtomClass>, AtomClass>();
    }

    // note: graph and vocabulary are not affected by this operation
    public void reset() {
        atomClassifications.clear();
    }

    public List<AtomClassEntry> getClassInfo(final Atom a) {
        return atomClassifications.get(a);
    }

    public void addDefaultClasses()
            throws InstantiationException, IllegalAccessException {

        Class[] vocabulary = new Class[]{
                // basic classes
                AKAReference.class,
                BibtexReference.class,
                Date.class,
                Document.class,
                ISBNReference.class,
                LinkedConcept.class,
                Person.class,
                RFIDReference.class,
                DatedEvent.class,
                TODOTask.class,
                URLReference.class,
                QuotedValue.class,
                WebPage.class,
                // context-specific classes
                DatedEvent.Birthday.class,
                // simple collections
                DocumentCollection.class,
                EventCollection.class,
                GenericCollection.class,
                PersonCollection.class,
                QuotedValueCollection.class,
                TODOCollection.class,
                // context-specific collections
                InterestCollection.class,
                NoteCollection.class
                // some classes still to add:
                //     Account, ManufacturedPart, Place, SoftwareProject
        };

        for (Class<? extends AtomClass> atomClass : vocabulary) {
            classes.put(atomClass, atomClass.newInstance());
        }
    }

    private void handleAllMembers(final AtomCollectionMemory memory,
                                  final AtomClass.FieldHandler fieldHandler,
                                  final RDFizationContext context,
                                  final Set<String> alreadyHandled) throws RDFHandlerException {
        // avoid cycles
        if (alreadyHandled.contains(memory.getAtomId())) {
            return;
        }
        alreadyHandled.add(memory.getAtomId());

        for (Atom a : memory.getMemberAtoms()) {
            fieldHandler.handle(a, context);
        }

        for (AtomCollectionMemory m : memory.getMemberCollections()) {
            handleAllMembers(m, fieldHandler, context, alreadyHandled);
        }
    }

    private boolean match(final Atom first,
                          final AtomRegex.El el,
                          final List<AtomClassEntry> evidenceEntries,
                          final AtomCollectionMemory memory,
                          final RDFizationContext context) throws RDFHandlerException {
        Set<Class<? extends AtomClass>> alts = el.getAlternatives();

        List<AtomClassEntry> entries = atomClassifications.get(first);
        if (null == entries) {
            // (as yet) unclassified atoms are only allowed to be trivial matches;
            // we don't attempt to rdfize them
            return 0 == alts.size();
        } else {
            for (AtomClassEntry inf : entries) {
                // note: if multiple class entries are acceptable, only the first will match, in greedy fashion.
                // The entries are sorted in descending order such that the highest-scoring is encountered first
                if (0 == alts.size() || alts.contains(inf.getInferredClass())) {
                    AtomClass atomClass = classes.get(inf.getInferredClass());

                    evidenceEntries.add(inf);

                    // generate RDF statements if an RDF handler has been provided
                    RDFHandler handler = context.getHandler();
                    if (null != handler) {
                        AtomClass.FieldHandler fieldHandler = el.getFieldHandler();

                        // fieldHandler is optional
                        if (null != fieldHandler) {
                            if (atomClass.isCollectionClass()) {
                                if (null != inf.memory) {
                                    handleAllMembers(inf.memory, fieldHandler, context, new HashSet<String>());
                                }
                            } else {
                                fieldHandler.handle(first, context);
                            }
                        }
                    }

                    // if the parent is in the process of being matched as a collection,
                    // add this member to the collection memory
                    if (null != memory) {
                        if (atomClass.isCollectionClass()) {
                            if (null != inf.memory) {
                                memory.getMemberCollections().add(inf.memory);
                            }
                        } else {
                            memory.getMemberAtoms().add(first);
                        }
                    }

                    return true;
                }
            }

            return false;
        }
    }

    public void inferClasses(final RDFHandler handler) throws RDFHandlerException {
        long startTime = System.currentTimeMillis();

        RDFBuffer buffer = null == handler ? null : new RDFBuffer(handler);
        ValueFactory vf = new ValueFactoryImpl(); // TODO
        RDFizationContext context = new RDFizationContext(buffer, vf);

        Comparator outScoreDescending = Collections.reverseOrder();
        Comparator totalScoreDescending = new AtomClassificationComparator();

        for (List<AtomClassEntry> l : atomClassifications.values()) {
            for (AtomClassEntry e : l) {
                e.inScore = 0;
            }
        }
        List<AtomClassEntry> evidenceEntries = new LinkedList<AtomClassEntry>();

        for (Atom a : graph.getAtoms()) {
            context.setSubject(a);

            String value = a.getValue();
            String alias = a.getAlias();

            if (null != buffer) {
                buffer.clear();
            }

            List<AtomClassEntry> oldEntries = atomClassifications.get(a);
            List<AtomClassEntry> newEntries = new LinkedList<AtomClassEntry>();

            for (AtomClass c : classes.values()) {
                /*
                if (a.asVertex().getId().equals("SBZFumn") && c.name.equals("person") && null != handler) {
                    System.out.println("break point here");
                }//*/

                evidenceEntries.clear();

                AtomCollectionMemory newMemo = c.isCollectionClass()
                        ? new AtomCollectionMemory((String) a.asVertex().getId())
                        : null;

                int points = 0;

                if (null != c.valueRegex) {
                    // one point for value regex
                    points++;
                    if (null == value || !c.valueRegex.matcher(value).matches()) {
                        continue;
                    }
                }

                if (null != c.aliasRegex) {
                    // one point for alias regex
                    points++;
                    if (null == alias || !c.aliasRegex.matcher(alias).matches()) {
                        continue;
                    }
                }

                if (null != c.memberRegex) {
                    AtomList cur = a.getNotes();
                    Atom first = null;
                    int eli = 0;
                    AtomRegex.El el = null;
                    AtomRegex.Modifier mod = null;
                    Set<Class<? extends AtomClass>> alts = null;
                    boolean advanceInput = true;
                    boolean advanceRegex = true;
                    boolean matched = false;
                    boolean fail = false;

                    while (!fail) { // break out on failure or exhaustion of the regex
                        if (advanceRegex) {
                            if (matched && null != alts) {
                                // one point for each wildcard regex element which matches at least one input
                                //     e.g. .? earns one point for "a", "b", "c" or any other atom, none for ""
                                // three points for each class-specific regex element which matches at least one input
                                //     e.g. A*(B|C)+ earns 3 points for "a" or "aa", 6 points for "aac"
                                points += alts.size() > 0 ? 3 : 1;
                            }
                            matched = false;

                            if (c.memberRegex.getElements().size() > eli) {
                                el = c.memberRegex.getElements().get(eli++);
                                mod = el.getModifier();
                                alts = el.getAlternatives();
                            } else {
                                // we need to have exhausted the input
                                if (null != cur) {
                                    fail = true;
                                }

                                break;
                            }

                            advanceRegex = false;
                        }

                        if (advanceInput) {
                            // we have exhausted the input
                            if (null == cur) {
                                if (AtomRegex.Modifier.One == mod || AtomRegex.Modifier.OneOrMore == mod) {
                                    // additional input is required by the regex; fail
                                    fail = true;
                                    break;
                                } else {
                                    // try to exhaust the regex
                                    advanceRegex = true;
                                    continue;
                                }
                            }
                            first = cur.getFirst();
                            cur = cur.getRest();

                            advanceInput = false;
                        }

                        switch (mod) {
                            case ZeroOrOne:
                                if (match(first, el, evidenceEntries, newMemo, context)) {
                                    advanceRegex = true;
                                    advanceInput = true;
                                    matched = true;
                                } else {
                                    advanceRegex = true;
                                }
                                break;
                            case ZeroOrMore:
                                if (match(first, el, evidenceEntries, newMemo, context)) {
                                    advanceInput = true;
                                    matched = true;
                                } else {
                                    advanceRegex = true;
                                }
                                break;
                            case One:
                                if (match(first, el, evidenceEntries, newMemo, context)) {
                                    advanceRegex = true;
                                    advanceInput = true;
                                    matched = true;
                                } else {
                                    fail = true;
                                }
                                break;
                            case OneOrMore:
                                if (match(first, el, evidenceEntries, newMemo, context)) {
                                    mod = AtomRegex.Modifier.ZeroOrMore;
                                    advanceInput = true;
                                    matched = true;
                                } else {
                                    fail = true;
                                }
                                break;
                        }
                    }

                    if (fail) {
                        continue;
                    }
                }

                // at this point, we have classified the atom

                // point value is a compromise between a model in which all scores range from 0 to 1,
                // favoring simple classes, and a model in which scores range from 1 to infinity,
                // favoring more complex ones.
                // This gives simple classes priority over simpler atoms (e.g. the ISBN class which simply
                // matches based on a value regex), which requires more complex classes to be matched by means
                // of graph structure.
                int poss = c.getHighestOutScore();
                float pointValue = (1 + poss) / (2.0f * poss);
                float outScore = points * pointValue;

                boolean updated = false;
                if (null != oldEntries) {
                    for (AtomClassEntry e : oldEntries) {
                        if (e.getInferredClass() == c.getClass()) {
                            e.outScore = outScore;
                            e.memory = newMemo;
                            newEntries.add(e);
                            updated = true;
                            break;
                        }
                    }
                }
                if (!updated) {
                    newEntries.add(new AtomClassEntry(c.getClass(), outScore, newMemo));
                }
                // augment relevant in-scores of member atoms
                for (AtomClassEntry e : evidenceEntries) {
                    e.inScore = outScore + e.getInScore();
                }

                if (null != buffer) {
                    c.toRDF(a, context);

                    // flush both the immediate description of the atom, as well as the relationship
                    // of the atom to its fields to the downstream handler.
                    buffer.flush();
                }
            }

            // replace old classification
            atomClassifications.remove(a);
            if (newEntries.size() > 0) {
                Collections.sort(newEntries, outScoreDescending);
                atomClassifications.put(a, newEntries);
            }
        }

        long typed = atomClassifications.size();
        long total = countAtoms();

        long endTime = System.currentTimeMillis();
        logger.info("typed " + typed + " of " + total + " atoms ("
                + (total - typed) + " remaining) in " + (endTime - startTime) + "ms");
    }

    private long countAtoms() {
        long count = 0;
        for (Atom a : graph.getAtoms()) {
            count++;
        }
        return count;
    }

    // development/convenience method
    public void viewInferred(final Atom a) {
        viewInferredInternal(a, 0);
    }

    public static class AtomClassificationComparator implements Comparator<KnowledgeBase.AtomClassEntry> {
        private static final AtomClassificationComparator INSTANCE = new AtomClassificationComparator();

        public int compare(KnowledgeBase.AtomClassEntry first, KnowledgeBase.AtomClassEntry second) {
            // descending order based on total score
            return ((Float) second.getScore()).compareTo(first.getScore());
        }
    }

    private void viewInferredInternal(final Atom a,
                                      int indent) {
        for (int i = 0; i < indent; i++) System.out.print("\t");
        String value = a.getValue();
        String value50 = null == value
                ? "[null]"
                : value.length() > 50
                ? value.substring(0, 50)
                : value;
        System.out.println("* :" + a.asVertex().getId() + ": " + value50);
        List<AtomClassEntry> entries = atomClassifications.get(a);
        if (null != entries) {
            List<AtomClassEntry> helper = new LinkedList<AtomClassEntry>();
            helper.addAll(entries);
            Collections.sort(helper, AtomClassificationComparator.INSTANCE);
            for (AtomClassEntry e : helper) {
                for (int i = 0; i <= indent; i++) System.out.print("\t");
                System.out.format("@(%s %.2f=%.2f+%.2f)\n",
                        e.getInferredClassName(), e.getScore(), e.getOutScore(), e.getInScore());
            }
        }
        indent++;
        if (indent < 2) {
            AtomList notes = a.getNotes();
            if (null != notes) {
                AtomList cur = notes;
                while (null != cur) {
                    viewInferredInternal(cur.getFirst(), indent);
                    cur = cur.getRest();
                }
            }
        }
    }

    // convenience method
    public void exportRDF(final OutputStream out) throws SailException, RDFHandlerException {
        long startTime, endTime;
        Sail dedupSail = new MemoryStore();
        dedupSail.initialize();
        try {
            SailConnection sc = dedupSail.getConnection();
            try {
                sc.begin();

                startTime = System.currentTimeMillis();
                RDFHandler h0 = new SailAdder(sc);
                h0.startRDF();

                inferClasses(h0);

                h0.endRDF();
                endTime = System.currentTimeMillis();
                logger.info("inferred classes and generated RDF in " + (endTime - startTime) + "ms");

                sc.commit();
                sc.begin();

                startTime = System.currentTimeMillis();
                RDFHandler h = Rio.createWriter(RDFFormat.NTRIPLES, out);
                h.startRDF();
                CloseableIteration<? extends Statement, SailException>
                        iter = sc.getStatements(null, null, null, false);
                try {
                    while (iter.hasNext()) {
                        h.handleStatement(iter.next());
                    }
                } finally {
                    iter.close();
                }
                h.endRDF();
                endTime = System.currentTimeMillis();
                logger.info("wrote triples to disk in " + (endTime - startTime) + "ms");
            } finally {
                sc.rollback();
                sc.close();
            }
        } finally {
            dedupSail.shutDown();
        }
    }

    private static class SailAdder implements RDFHandler {
        private final SailConnection sc;

        private SailAdder(SailConnection sc) {
            this.sc = sc;
        }

        @Override
        public void startRDF() throws RDFHandlerException {
        }

        @Override
        public void endRDF() throws RDFHandlerException {
        }

        @Override
        public void handleNamespace(String s, String s2) throws RDFHandlerException {
        }

        @Override
        public void handleStatement(Statement statement) throws RDFHandlerException {
            try {
                sc.addStatement(
                        statement.getSubject(),
                        statement.getPredicate(),
                        statement.getObject(),
                        statement.getContext());
            } catch (SailException e) {
                throw new RDFHandlerException(e);
            }
        }

        @Override
        public void handleComment(String s) throws RDFHandlerException {
        }
    }

    private class RDFBuffer implements RDFHandler {
        private final RDFHandler wrappedHandler;
        private final Collection<Statement> buffer = new LinkedList<Statement>();

        private RDFBuffer(final RDFHandler wrappedHandler) {
            this.wrappedHandler = wrappedHandler;
        }

        public void startRDF() throws RDFHandlerException {
            wrappedHandler.startRDF();
        }

        public void endRDF() throws RDFHandlerException {
            wrappedHandler.endRDF();
        }

        public void handleNamespace(String s, String s2) throws RDFHandlerException {
            wrappedHandler.handleNamespace(s, s2);
        }

        public void handleStatement(final Statement statement) throws RDFHandlerException {
            buffer.add(statement);
        }

        public void handleComment(String s) throws RDFHandlerException {
            wrappedHandler.handleComment(s);
        }

        public void flush() throws RDFHandlerException {
            try {
                for (Statement s : buffer) {
                    wrappedHandler.handleStatement(s);
                }
            } finally {
                buffer.clear();
            }
        }

        public void clear() {
            buffer.clear();
        }
    }

    public class AtomClassEntry implements Comparable<AtomClassEntry> {
        private final Class<? extends AtomClass> inferredClass;
        private float outScore;
        private float inScore;
        private AtomCollectionMemory memory;

        public AtomClassEntry(Class<? extends AtomClass> inferredClass, float outScore, AtomCollectionMemory memory) {
            this.inferredClass = inferredClass;
            this.outScore = outScore;
            this.memory = memory;
        }

        public Class<? extends AtomClass> getInferredClass() {
            return inferredClass;
        }

        public String getInferredClassName() {
            return classes.get(inferredClass).name;
        }

        public float getOutScore() {
            return outScore;
        }

        // compare based on out-score alone.  Used for the first stage of classification
        public int compareTo(AtomClassEntry other) {
            return ((Float) outScore).compareTo(other.outScore);
        }

        public float getInScore() {
            return inScore;
        }

        public float getScore() {
            return inScore + outScore;
        }
    }
}
