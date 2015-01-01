package net.fortytwo.extendo.brain.rdf;

import info.aduna.iteration.CloseableIteration;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.AtomList;
import net.fortytwo.extendo.brain.BrainGraph;
import net.fortytwo.extendo.brain.Filter;
import net.fortytwo.extendo.brain.rdf.classes.AKAReference;
import net.fortytwo.extendo.brain.rdf.classes.AbstractEvent;
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
import net.fortytwo.extendo.brain.rdf.classes.Tool;
import net.fortytwo.extendo.brain.rdf.classes.URLReference;
import net.fortytwo.extendo.brain.rdf.classes.Usage;
import net.fortytwo.extendo.brain.rdf.classes.WebPage;
import net.fortytwo.extendo.brain.rdf.classes.collections.AttendedEventsCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.DocumentCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.Log;
import net.fortytwo.extendo.brain.rdf.classes.collections.GenericCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.PersonCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.QuotedValueCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.TODOCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.TopicCollection;
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
 * An inference layer for an Extend-o-Brain graph, supporting automatic classification of atoms and exporting to RDF
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class KnowledgeBase {
    private static final Logger logger = Extendo.getLogger(KnowledgeBase.class);

    private final BrainGraph graph;

    private final Map<Class<? extends AtomClass>, AtomClass> classes;

    private final Map<Atom, List<AtomClassEntry>> atomClassifications;

    private ValueFactory valueFactory = new ValueFactoryImpl();

    public KnowledgeBase(final BrainGraph graph) {
        this.graph = graph;
        this.atomClassifications = new HashMap<Atom, List<AtomClassEntry>>();
        this.classes = new HashMap<Class<? extends AtomClass>, AtomClass>();
    }

    /**
     * Overrides the default ValueFactory
     * @param valueFactory a new ValueFactory to generate RDF statements and basic values
     */
    public void setValueFactory(final ValueFactory valueFactory) {
        this.valueFactory = valueFactory;
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
                AbstractEvent.class,
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
                Tool.class,
                URLReference.class,
                Usage.class,
                QuotedValue.class,
                WebPage.class,
                // context-specific classes
                DatedEvent.Birthday.class,
                // simple collections
                DocumentCollection.class,
                Log.class,
                GenericCollection.class,
                PersonCollection.class,
                QuotedValueCollection.class,
                TODOCollection.class,
                // context-specific collections
                AttendedEventsCollection.class,
                TopicCollection.class,
                AbstractEvent.InteractorCollection.class,
                Document.NoteCollection.class,
                Document.AuthorCollection.class,
                Person.WorksCollection.class,
                Person.InterestsCollection.class,
                Person.SocialNetworkCollection.class,
                Tool.ContributorCollection.class,
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
                                  final Set<String> alreadyHandled,
                                  final Filter filter) throws RDFHandlerException {
        // avoid cycles
        if (alreadyHandled.contains(memory.getAtomId())) {
            return;
        }
        alreadyHandled.add(memory.getAtomId());

        for (Atom a : memory.getMemberAtoms()) {
            if (null == filter || filter.isVisible(a.asVertex())) {
                fieldHandler.handle(a, context);
            }
        }

        for (AtomCollectionMemory m : memory.getMemberCollections()) {
            handleAllMembers(m, fieldHandler, context, alreadyHandled, filter);
        }
    }

    private interface RdfizationCallback {
        void execute() throws RDFHandlerException;
    }

    /*
    Matches the children of an atom against an atom regex element (class or wildcard with quantifier)
     */
    private boolean match(final Atom childAtom,
                          final AtomRegex.El el,
                          final List<AtomClassEntry> evidenceEntries,
                          final AtomCollectionMemory memory,
                          final RDFizationContext context,
                          final Collection<RdfizationCallback> callbacks,
                          final Filter filter) throws RDFHandlerException {
        Set<Class<? extends AtomClass>> alts = el.getAlternatives();

        List<AtomClassEntry> entries = atomClassifications.get(childAtom);
        if (null == entries) { // unclassified
            // The unclassified atom matches if the element has no alternatives, i.e. accepts everything.
            // note: (as yet) unclassified atoms are only allowed to be trivial matches;
            // we don't attempt to rdfize them
            return 0 == alts.size();
        } else { // one or more classes
            for (final AtomClassEntry entry : entries) {
                // note: if multiple class entries are acceptable, only the first will match, in greedy fashion.
                // The entries are sorted in descending order such that one with the highest out-score,
                // or self-classification, is encountered first
                if (0 == alts.size() || alts.contains(entry.getInferredClass())) {
                    final AtomClass atomClass = classes.get(entry.getInferredClass());

                    // only add evidence for specifically matched classes
                    if (0 < alts.size()) {
                        evidenceEntries.add(entry);
                    }

                    // add an rdfization callback which will be executed if and only if the current classification is
                    // chosen for the parent atom.  Delaying execution avoids multiple-typing of atoms,
                    // or the wasted effort of generating RDF statements which are not allowed in the output.
                    if (null != callbacks) {
                        final AtomClass.FieldHandler fieldHandler = el.getFieldHandler();

                        // fieldHandler is optional
                        if (null != fieldHandler) {
                            callbacks.add(new RdfizationCallback() {
                                @Override
                                public void execute() throws RDFHandlerException {
                                    if (atomClass.isCollectionClass()) {
                                        if (null != entry.memory) {
                                            handleAllMembers(entry.memory, fieldHandler, context,
                                                    new HashSet<String>(), filter);
                                        }
                                    } else if (null == filter || filter.isVisible(childAtom.asVertex())) {
                                        fieldHandler.handle(childAtom, context);
                                    }
                                }
                            });
                        }
                    }

                    // if the parent is in the process of being matched as a collection,
                    // add this member to the collection memory
                    if (null != memory) {
                        if (atomClass.isCollectionClass()) {
                            if (null != entry.memory) {
                                memory.getMemberCollections().add(entry.memory);
                            }
                        } else {
                            memory.getMemberAtoms().add(childAtom);
                        }
                    }

                    return true;
                }
            }

            return false;
        }
    }

    /**
     * Performs Extendo type inference on the knowledge base, optionally generating an RDF representation
     * @param handler a handler for generated RDF statements (may be null)
     * @param filter an optional sharability filter for generated results.
     *               Type inference is performed on the entire knowledge base without regard to sharability,
     *               but generated RDF statements are limited to those subjects which are sharable according to
     *               the filter.
     */
    public void inferClasses(final RDFHandler handler, final Filter filter) throws RDFHandlerException {
        long startTime = System.currentTimeMillis();

        RDFizationContext context = new RDFizationContext(handler, valueFactory);

        // class entries are sorted in descending order based on out-score rather than total score so as to avoid
        // feedback -- see match().  The final score for a class and atom is the sum of out-score and in-score.
        Comparator outScoreDescending = Collections.reverseOrder();
        Comparator totalScoreDescending = new AtomClassificationComparator();

        // classify or re-classify each atom
        for (Atom subject : graph.getAtoms()) {
            context.setSubject(subject);

            String value = subject.getValue();
            String alias = subject.getAlias();

            List<AtomClassEntry> oldEntries = atomClassifications.get(subject);
            List<AtomClassEntry> newEntries = new LinkedList<AtomClassEntry>();

            for (AtomClass clazz : classes.values()) {
                /* DO NOT REMOVE
                if (subject.asVertex().getId().equals("yOXFhhN") && clazz.name.equals("person")) {// && null != handler) {
                    System.out.println("break point here");
                }//*/

                List<AtomClassEntry> evidenceEntries = new LinkedList<AtomClassEntry>();

                Collection<RdfizationCallback> callbacks = null == handler
                        ? null : new LinkedList<RdfizationCallback>();

                AtomCollectionMemory newMemo = clazz.isCollectionClass()
                        ? new AtomCollectionMemory((String) subject.asVertex().getId())
                        : null;


                if (null != clazz.valueRegex) {
                    if (null == value || !clazz.valueRegex.matcher(value).matches()) {
                        continue;
                    }
                }

                if (null != clazz.aliasRegex) {
                    if (null == alias || !clazz.aliasRegex.matcher(alias).matches()) {
                        continue;
                    }
                }

                // out-score is the number of ways in which the member regex of the atom matches
                // out-score is not affected by the value or alias regex, as these are considered necessary
                // but not sufficient for classification
                int outScore = 0;

                if (null != clazz.memberRegex) {
                    AtomList cur = subject.getNotes();
                    Atom first = null;
                    int eli = 0;
                    AtomRegex.El el = null;
                    AtomRegex.Modifier mod = null;
                    Set<Class<? extends AtomClass>> alts = null;
                    boolean advanceInput = true;
                    boolean advanceRegex = true;
                    boolean matched = false;
                    boolean fail = false;

                    // break out on failure or exhaustion of the regex
                    while (!fail) {
                        // assign one point per matched input element (as opposed to only one per regex element)
                        if (matched && null != alts) {
                            // assign a point only if the regex element matches a specific class, not a wildcard
                            if (alts.size() > 0) {
                                outScore++;
                            }
                            matched = false;
                        }

                        if (advanceRegex) {
                            if (clazz.memberRegex.getElements().size() > eli) {
                                el = clazz.memberRegex.getElements().get(eli++);
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
                            if (null == cur) {
                                // we have exhausted the input
                                if (AtomRegex.Modifier.One == mod || AtomRegex.Modifier.OneOrMore == mod) {
                                    // additional input is required by the regex; fail
                                    fail = true;
                                    break;
                                } else {
                                    // try to exhaust the regex without further input
                                    advanceRegex = true;
                                    advanceInput = false;
                                    continue;
                                }
                            } else {
                                first = cur.getFirst();
                                cur = cur.getRest();
                                advanceInput = false;
                            }
                        }

                        switch (mod) {
                            case ZeroOrOne:
                                if (match(first, el, evidenceEntries, newMemo, context, callbacks, filter)) {
                                    advanceRegex = true;
                                    advanceInput = true;
                                    matched = true;
                                } else {
                                    advanceRegex = true;
                                }
                                break;
                            case ZeroOrMore:
                                if (match(first, el, evidenceEntries, newMemo, context, callbacks, filter)) {
                                    advanceInput = true;
                                    matched = true;
                                } else {
                                    advanceRegex = true;
                                }
                                break;
                            case One:
                                if (match(first, el, evidenceEntries, newMemo, context, callbacks, filter)) {
                                    advanceRegex = true;
                                    advanceInput = true;
                                    matched = true;
                                } else {
                                    fail = true;
                                }
                                break;
                            case OneOrMore:
                                if (match(first, el, evidenceEntries, newMemo, context, callbacks, filter)) {
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

                // update or create the atom's entry for this class.
                // It is necessary to preserve an existing entry, if any, for the sake of the in-score
                AtomClassEntry classEntry = null;
                if (null != oldEntries) {
                    for (AtomClassEntry e : oldEntries) {
                        if (e.getInferredClass() == clazz.getClass()) {
                            e.outScore = outScore;
                            e.memory = newMemo;
                            classEntry = e;
                            break;
                        }
                    }
                }
                if (null == classEntry) {
                    classEntry = new AtomClassEntry(clazz.getClass(), outScore, newMemo);
                }
                classEntry.callbacks = callbacks;
                newEntries.add(classEntry);

                // augment relevant in-scores of member atoms
                for (AtomClassEntry e : evidenceEntries) {
                    e.futureInScore += 1;
                }
            }

            // remove old classification (if any) and replace with the new one (if any)
            atomClassifications.remove(subject);
            if (newEntries.size() > 0) {
                Collections.sort(newEntries, outScoreDescending);
                atomClassifications.put(subject, newEntries);
            }

            // perform rdfization, choosing at most one classification
            if (null != handler) {
                if (newEntries.size() > 0) {
                    List<AtomClassEntry> helper = new LinkedList<AtomClassEntry>();
                    helper.addAll(newEntries);
                    Collections.sort(helper, totalScoreDescending);
                    AtomClassEntry best = helper.get(0);
                    // A classification which has a score of zero is a trivial property-based regex match with no
                    // structural evidence to support it.
                    // These are not included in the exported RDF, as they include many false positives.
                    if (best.getScore() > 0) {
                        AtomClass clazz = classes.get(best.getInferredClass());
                        clazz.toRDF(subject, context);
                        for (RdfizationCallback callback : best.callbacks) {
                            callback.execute();
                        }
                    }
                }
            }
        }

        // update all in-scores, globally, and clear future in-scores in preparation for the next iteration
        for (List<AtomClassEntry> l : atomClassifications.values()) {
            for (AtomClassEntry e : l) {
                e.inScore = e.futureInScore;
                e.futureInScore = 0;
                // also clear callbacks to free memory
                e.callbacks = null;
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

    /**
     * Prints a representation of the class inference results for a given atom to standard output.
     * This is a development/convenience method.
     * @param a the atom to view
     */
    public void viewInferred(final Atom a) {
        viewInferredInternal(a, 0);
    }

    public static class AtomClassificationComparator implements Comparator<KnowledgeBase.AtomClassEntry> {
        public static final AtomClassificationComparator INSTANCE = new AtomClassificationComparator();

        public int compare(KnowledgeBase.AtomClassEntry first, KnowledgeBase.AtomClassEntry second) {
            // descending order based on total score.  Resolve a tie by lexicographical order of class names.
            int cmp = ((Integer) second.getScore()).compareTo(first.getScore());
            return 0 == cmp
                    ? first.getInferredClassName().compareTo(second.getInferredClassName()) : cmp;
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
                System.out.println("@(" + e.getInferredClassName()
                        + " " + e.getScore() + "=" + e.getOutScore() + "+" + e.getInScore() + ")");
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

    public void exportRDF(final OutputStream out,
                          final RDFFormat format,
                          final Filter filter) throws SailException, RDFHandlerException {
        logger.info("exporting RDF in format " + format);
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

                inferClasses(h0, filter);

                h0.endRDF();
                endTime = System.currentTimeMillis();
                logger.info("inferred classes and generated RDF in " + (endTime - startTime) + "ms");

                sc.commit();
                sc.begin();

                startTime = System.currentTimeMillis();
                RDFHandler h = Rio.createWriter(format, out);
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

    public class AtomClassEntry implements Comparable<AtomClassEntry> {
        private final Class<? extends AtomClass> inferredClass;
        private int outScore;
        private int inScore;
        private int futureInScore;
        private AtomCollectionMemory memory;
        private Collection<RdfizationCallback> callbacks;

        public AtomClassEntry(Class<? extends AtomClass> inferredClass, int outScore, AtomCollectionMemory memory) {
            this.inferredClass = inferredClass;
            this.outScore = outScore;
            this.memory = memory;

            // note explicitly that in-score and future in-score (the working score) start at 0 for each entry
            inScore = 0;
            futureInScore = 0;
        }

        public Class<? extends AtomClass> getInferredClass() {
            return inferredClass;
        }

        public String getInferredClassName() {
            return classes.get(inferredClass).name;
        }

        public int getOutScore() {
            return outScore;
        }

        // compare based on out-score alone.  Used for the first stage of classification
        public int compareTo(AtomClassEntry other) {
            return ((Integer) outScore).compareTo(other.outScore);
        }

        public int getInScore() {
            return inScore;
        }

        public int getScore() {
            return inScore + outScore;
        }
    }
}
