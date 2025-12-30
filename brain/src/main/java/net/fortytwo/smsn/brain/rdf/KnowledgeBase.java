package net.fortytwo.smsn.brain.rdf;

import org.eclipse.rdf4j.common.iteration.CloseableIteration;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.rdf.classes.*;
import net.fortytwo.smsn.brain.rdf.classes.Date;
import net.fortytwo.smsn.brain.rdf.classes.collections.*;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.eclipse.rdf4j.rio.RDFHandler;
import org.eclipse.rdf4j.rio.RDFHandlerException;
import org.eclipse.rdf4j.rio.Rio;
import org.eclipse.rdf4j.sail.Sail;
import org.eclipse.rdf4j.sail.SailConnection;
import org.eclipse.rdf4j.sail.SailException;
import org.eclipse.rdf4j.sail.memory.MemoryStore;

import java.io.OutputStream;
import java.util.*;
import java.util.logging.Level;

/**
 * An inference layer for an Extend-o-Brain graph, supporting automatic classification of notes and exporting to RDF
 */
public class KnowledgeBase {

    private final TopicGraph topicGraph;

    private final Map<Class<? extends NoteClass>, NoteClass> classes;

    private final Map<Note, List<NoteClassEntry>> noteClassifications;

    private ValueFactory valueFactory = SimpleValueFactory.getInstance();

    public KnowledgeBase(final TopicGraph topicGraph) {
        this.topicGraph = topicGraph;
        this.noteClassifications = new HashMap<>();
        this.classes = new HashMap<>();
    }

    /**
     * Overrides the default ValueFactory
     *
     * @param valueFactory a new ValueFactory to generate RDF statements and basic values
     */
    public void setValueFactory(final ValueFactory valueFactory) {
        this.valueFactory = valueFactory;
    }

    // note: graph and vocabulary are not affected by this operation
    public synchronized void reset() {
        noteClassifications.clear();
    }

    /**
     * Gets a list of classifications of the given note, sorted in descending order by score.
     * If the note has not been classified, a null is returned.
     *
     * @param a the classified note
     * @return either null (if the note has not been classified)
     * or a list of classifications of the given note, sorted in descending order by score
     */
    public List<NoteClassEntry> getClassInfo(final Note a) {
        List<NoteClassEntry> entries = noteClassifications.get(a);

        if (null == entries || 0 == entries.size()) {
            return entries;
        } else {
            // sort in descending order by total score, putting the top-ranked class first
            List<NoteClassEntry> helper = new java.util.LinkedList<>();
            helper.addAll(entries);
            Collections.sort(helper, NoteClassificationComparator.INSTANCE);
            return helper;
        }
    }

    public void addDefaultClasses()
            throws InstantiationException, IllegalAccessException {

        Class[] vocabulary = new Class[]{
                // basic classes
                AbstractEvent.class,
                AKAReference.class,
                BibtexEntry.class,
                BibtexReference.class,
                Date.class,
                Document.class,
                ISBNReference.class,
                LinkedConcept.class,
                Organization.class,
                Person.class,
                RFIDReference.class,
                DatedEvent.class,
                TODOTask.class,
                Tool.class,
                URLReference.class,
                Usage.class,
                QuotedValue.class,
                WebPage.class,
                Topic.class,
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
                DocumentAboutTopicCollection.class,
                AbstractEvent.InteractorCollection.class,
                Document.NoteCollection.class,
                Document.AuthorCollection.class,
                Person.WorksCollection.class,
                Person.InterestsCollection.class,
                Person.SocialNetworkCollection.class,
                Person.PersonalEventsCollection.class,
                Person.PersonalStuffCollection.class,
                Person.BelongingsCollection.class,
                Tool.ContributorCollection.class,
                // some classes still to add:
                //     Account, ManufacturedPart, Place, SoftwareProject
        };

        for (Class<? extends NoteClass> noteClass : vocabulary) {
            classes.put(noteClass, noteClass.newInstance());
        }
    }


    public void inferAutomatically(final long initialWait, final long interval) {
        final int totalSteps = 4;
        new Thread(() -> {
            try {
                Thread.sleep(initialWait);
            } catch (InterruptedException e) {
                SemanticSynchrony.getLogger().log(Level.WARNING, "interrupted", e);
            }

            for (int i = 0; i < totalSteps; i++) {
                try {
                    SemanticSynchrony.getLogger().info("performing warm-up inference step #" + (i + 1) + "/" + totalSteps);
                    inferClasses(null, null);
                } catch (RDFHandlerException e) {
                    SemanticSynchrony.getLogger().log(Level.WARNING, "error in warm-up inference", e);
                }
            }
            SemanticSynchrony.getLogger().info("completed warm-up inference");

            long lastUpdate = topicGraph.getLastUpdate();

            while (true) {
                try {
                    Thread.sleep(interval);
                } catch (InterruptedException e) {
                    break;
                }

                // only repeat the inference step if there have been updates in the meantime
                long u = topicGraph.getLastUpdate();
                if (u > lastUpdate) {
                    try {
                        SemanticSynchrony.getLogger().info("performing class inference");
                        inferClasses(null, null);
                    } catch (RDFHandlerException e) {
                        SemanticSynchrony.getLogger().log(Level.WARNING, "class inference failed. Will keep trying", e);
                    }
                    lastUpdate = u;
                }
            }
        }).start();
    }

    private void handleAllMembers(final NoteCollectionMemory memory,
                                  final NoteClass.FieldHandler fieldHandler,
                                  final RDFizationContext context,
                                  final Set<AtomId> alreadyHandled,
                                  final Filter filter) throws RDFHandlerException {
        // avoid cycles
        if (alreadyHandled.contains(memory.getNoteId())) {
            return;
        }
        alreadyHandled.add(memory.getNoteId());

        // only rdfize fields with a known class
        memory.getMemberNotes().stream().filter(a -> null == filter || filter.test(a)).forEach(a -> {
            // only rdfize fields with a known class
            if (isClassified(a)) {
                fieldHandler.handle(a, context);
            }
        });

        for (NoteCollectionMemory m : memory.getMemberCollections()) {
            handleAllMembers(m, fieldHandler, context, alreadyHandled, filter);
        }
    }

    private interface RdfizationCallback {
        void execute() throws RDFHandlerException;
    }

    private enum MatchResult {Unclassified, Supported, Unsupported, NoMatch}

    private boolean isClassified(final List<NoteClassEntry> entries) {
        if (null == entries || 0 == entries.size()) {
            return false;
        } else {
            for (NoteClassEntry e : entries) {
                if (e.getScore() > 0) {
                    return true;
                }
            }
            return false;
        }
    }

    private boolean isClassified(final Note note) {
        /*
        if (note.asVertex().getId().equals("ynyUshJ")) {
            System.out.println("break here");
        }*/
        List<NoteClassEntry> entries = noteClassifications.get(note);
        return isClassified(entries);
    }

    /*
    Matches the children of a note against a note regex element (class or wildcard with quantifier)
     */
    private MatchResult match(final Note childNote,
                              final NoteReqex.El el,
                              final List<NoteClassEntry> evidenceEntries,
                              final NoteCollectionMemory memory,
                              final RDFizationContext context,
                              final Collection<RdfizationCallback> callbacks,
                              final Filter filter) throws RDFHandlerException {
        Set<Class<? extends NoteClass>> alts = el.getAlternatives();

        final List<NoteClassEntry> entries = noteClassifications.get(childNote);
        if (null == entries) { // unclassified
            // The unclassified note matches if the element has no alternatives, i.e. accepts everything.
            // note: (as yet) unclassified notes are only allowed to be trivial matches;
            // we don't attempt to rdfize them
            return 0 == alts.size() ? MatchResult.Unclassified : MatchResult.NoMatch;
        } else { // one or more classes
            for (final NoteClassEntry entry : entries) {
                // note: if multiple class entries are acceptable, only the first will match, in greedy fashion.
                // The entries are sorted in descending order such that one with the highest out-score,
                // or self-classification, is encountered first
                if (0 == alts.size() || alts.contains(entry.getInferredClass())) {
                    final NoteClass noteClass = classes.get(entry.getInferredClass());

                    // only add evidence for specifically matched classes,
                    // omitting evidence if the element is scored as a wildcard.
                    if (el.getWeight() > 0) {
                        evidenceEntries.add(entry);
                    }

                    // add an rdfization callback which will be executed if and only if the current classification is
                    // chosen for the parent note.  Delaying execution avoids multiple-typing of notes,
                    // or the wasted effort of generating RDF statements which are not allowed in the output.
                    if (null != callbacks) {
                        final NoteClass.FieldHandler fieldHandler = el.getFieldHandler();

                        // fieldHandler is optional
                        if (null != fieldHandler) {
                            callbacks.add(() -> {
                                if (noteClass.isCollectionClass()) {
                                    if (null != entry.memory) {
                                        handleAllMembers(entry.memory, fieldHandler, context,
                                                new HashSet<>(), filter);
                                    }
                                } else if (null == filter || filter.test(childNote)) {
                                    // only rdfize fields with a known class
                                    if (isClassified(entries)) {
                                        fieldHandler.handle(childNote, context);
                                    }
                                }
                            });
                        }
                    }

                    // if the parent is in the process of being matched as a collection,
                    // add this member to the collection memory
                    if (null != memory) {
                        if (noteClass.isCollectionClass()) {
                            if (null != entry.memory) {
                                memory.getMemberCollections().add(entry.memory);
                            }
                        } else {
                            memory.getMemberNotes().add(childNote);
                        }
                    }

                    return entry.getOutScore() > 0 ? MatchResult.Supported : MatchResult.Unsupported;
                }
            }

            return MatchResult.NoMatch;
        }
    }

    /**
     * Performs SmSn type inference on the knowledge base, optionally generating an RDF representation
     *
     * @param handler a handler for generated RDF statements (may be null)
     * @param filter  an optional filter for generated results.
     *                Type inference is performed on the entire knowledge base without regard to source,
     *                but generated RDF statements are limited to those subjects which are sharable according to
     *                the filter.
     * @throws org.eclipse.rdf4j.rio.RDFHandlerException if a downstream error occurs
     */
    public synchronized void inferClasses(final RDFHandler handler, final Filter filter) throws RDFHandlerException {
        long startTime = System.currentTimeMillis();

        RDFizationContext context = new RDFizationContext(topicGraph, handler, valueFactory);

        // class entries are sorted in descending order based on out-score rather than total score so as to avoid
        // feedback -- see match().  The final score for a class and note is the sum of out-score and in-score.
        Comparator outScoreDescending = Collections.reverseOrder();
        Comparator totalScoreDescending = new NoteClassificationComparator();

        // classify or re-classify each note
        for (Note subject : topicGraph.getAllNotes()) {
            context.setSubject(subject);

            String value = Note.getTitle(subject);
            String alias = Note.getAlias(subject);

            List<NoteClassEntry> oldEntries = noteClassifications.get(subject);
            List<NoteClassEntry> newEntries = new java.util.LinkedList<>();

            for (NoteClass clazz : classes.values()) {
                /* DO NOT REMOVE
                if (subject.asVertex().getId().equals("0rYY9z0") && clazz.name.equals("person")) {// && null != handler) {
                    System.out.println("break point here");
                }//*/

                List<NoteClassEntry> evidenceEntries = new java.util.LinkedList<>();

                Collection<RdfizationCallback> callbacks = null == handler
                        ? null : new java.util.LinkedList();

                NoteCollectionMemory memory = clazz.isCollectionClass()
                        ? new NoteCollectionMemory(Note.getId(subject))
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

                // out-score is the number of ways in which the member regex of the note matches
                // out-score is not affected by the value or alias regex, as these are considered necessary
                // but not sufficient for classification
                int outScore = 0;

                if (null != clazz.memberRegex) {
                    ListNode<Note> cur = subject.getChildren();
                    Note first = null;
                    int eli = 0;
                    NoteReqex.El el = null;
                    NoteReqex.Modifier mod = null;
                    boolean advanceInput = true;
                    boolean advanceRegex = true;
                    boolean matched;
                    boolean fail = false;

                    // break out on failure or exhaustion of the regex
                    while (!fail) {
                        if (advanceRegex) {
                            if (clazz.memberRegex.getElements().size() > eli) {
                                el = clazz.memberRegex.getElements().get(eli++);
                                mod = el.getModifier();
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
                                if (NoteReqex.Modifier.One == mod || NoteReqex.Modifier.OneOrMore == mod) {
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

                        MatchResult matchResult = match(first, el, evidenceEntries, memory, context, callbacks, filter);

                        // assign points per matched input element (rather than only per regex element)
                        switch (matchResult) {
                            case Unsupported:
                                // Assign a point only if the regex element matches a specific class, not a wildcard,
                                // and the current element has not been marked to score as a wildcard.
                                // Particularly significant elements may score extra points.
                                outScore += el.getWeight();
                                break;
                            case Supported:
                                // Members supported by internal evidence (i.e. having non-zero out-scores)
                                // in turn support the parent more strongly than members which merely satisfy
                                // the property constraints.
                                outScore += el.getWeight() * 2;
                                break;
                            case Unclassified: // fall through
                            case NoMatch:
                                break;
                        }

                        matched = matchResult != MatchResult.NoMatch;

                        switch (mod) {
                            case ZeroOrOne:
                                if (matched) {
                                    advanceRegex = true;
                                    advanceInput = true;
                                } else {
                                    advanceRegex = true;
                                }
                                break;
                            case ZeroOrMore:
                                if (matched) {
                                    advanceInput = true;
                                } else {
                                    advanceRegex = true;
                                }
                                break;
                            case One:
                                if (matched) {
                                    advanceRegex = true;
                                    advanceInput = true;
                                } else {
                                    fail = true;
                                }
                                break;
                            case OneOrMore:
                                if (matched) {
                                    mod = NoteReqex.Modifier.ZeroOrMore;
                                    advanceInput = true;
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

                // at this point, we have classified the note

                // update or create the note's entry for this class.
                // It is necessary to preserve an existing entry, if any, for the sake of the in-score
                NoteClassEntry classEntry = null;
                if (null != oldEntries) {
                    for (NoteClassEntry e : oldEntries) {
                        if (e.getInferredClass() == clazz.getClass()) {
                            e.outScore = outScore;
                            e.memory = memory;
                            classEntry = e;
                            break;
                        }
                    }
                }
                if (null == classEntry) {
                    classEntry = new NoteClassEntry(clazz.getClass(), outScore, memory);
                }
                classEntry.callbacks = callbacks;
                newEntries.add(classEntry);

                // augment relevant in-scores of member notes
                for (NoteClassEntry e : evidenceEntries) {
                    e.futureInScore += 1;
                }
            }

            // remove old classification (if any) and replace with the new one (if any)
            noteClassifications.remove(subject);
            if (newEntries.size() > 0) {
                Collections.sort(newEntries, outScoreDescending);
                noteClassifications.put(subject, newEntries);
            }

            // perform rdfization, choosing at most one classification
            if (null != handler && (null == filter || filter.test(subject))) {
                if (newEntries.size() > 0) {
                    List<NoteClassEntry> helper = new java.util.LinkedList<>();
                    helper.addAll(newEntries);
                    Collections.sort(helper, totalScoreDescending);
                    NoteClassEntry best = helper.get(0);
                    if (best.isNonTrivial()) {
                        NoteClass clazz = classes.get(best.getInferredClass());
                        clazz.toRDF(subject, context);
                        best.callbacks.forEach(RdfizationCallback::execute);
                    }
                }
            }
        }

        // update all in-scores, globally, and clear future in-scores in preparation for the next iteration
        for (List<NoteClassEntry> l : noteClassifications.values()) {
            for (NoteClassEntry e : l) {
                e.inScore = e.futureInScore;
                e.futureInScore = 0;
                // also clear callbacks to free memory
                e.callbacks = null;
            }
        }

        long typed = noteClassifications.size();
        long total = countNotes();

        long endTime = System.currentTimeMillis();
        SemanticSynchrony.getLogger().info("classified " + typed + " of " + total + " notes ("
                + (total - typed) + " remaining) in " + (endTime - startTime) + "ms");
    }

    private long countNotes() {
        long count = 0;
        for (Note a : topicGraph.getAllNotes()) {
            count++;
        }
        return count;
    }

    /**
     * Prints a representation of the class inference results for a given note to standard output.
     * This is a development/convenience method.
     *
     * @param a the note to view
     */
    public void viewInferred(final Note a) {
        viewInferredInternal(a, 0);
    }

    private void viewInferredInternal(final Note a,
                                      int indent) {
        for (int i = 0; i < indent; i++) System.out.print("\t");
        String value = Note.getTitle(a);
        String value50 = null == value
                ? "[null]"
                : value.length() > 50
                ? value.substring(0, 50)
                : value;
        System.out.println("* :" + Note.getId(a) + ": " + value50);
        List<NoteClassEntry> entries = noteClassifications.get(a);
        if (null != entries) {
            List<NoteClassEntry> helper = new java.util.LinkedList();
            helper.addAll(entries);
            Collections.sort(helper, NoteClassificationComparator.INSTANCE);
            for (NoteClassEntry e : helper) {
                for (int i = 0; i <= indent; i++) System.out.print("\t");
                System.out.println("@(" + e.getInferredClassName()
                        + " " + e.getScore() + "=" + e.getOutScore() + "+" + e.getInScore() + ")");
            }
        }
        indent++;
        if (indent < 2) {
            ListNode<Note> notes = a.getChildren();
            if (null != notes) {
                ListNode<Note> cur = notes;
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
        SemanticSynchrony.getLogger().info("exporting RDF in format " + format);
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
                SemanticSynchrony.getLogger().info("inferred classes and generated RDF in " + (endTime - startTime) + "ms");

                sc.commit();
                sc.begin();

                startTime = System.currentTimeMillis();
                RDFHandler h = Rio.createWriter(format, out);
                h.startRDF();
                try (CloseableIteration<? extends Statement, SailException> iter
                             = sc.getStatements(null, null, null, false)) {
                    while (iter.hasNext()) {
                        h.handleStatement(iter.next());
                    }
                }
                h.endRDF();
                endTime = System.currentTimeMillis();
                SemanticSynchrony.getLogger().info("wrote triples to disk in " + (endTime - startTime) + "ms");
            } finally {
                sc.rollback();
                sc.close();
            }
        } finally {
            dedupSail.shutDown();
        }
    }

    private static class NoteClassificationComparator implements Comparator<NoteClassEntry> {
        public static final NoteClassificationComparator INSTANCE = new NoteClassificationComparator();

        public int compare(NoteClassEntry first, NoteClassEntry second) {
            // descending order based on total score.  Resolve a tie by lexicographical order of class names.
            int cmp = ((Integer) second.getScore()).compareTo(first.getScore());
            return 0 == cmp
                    ? first.getInferredClassName().compareTo(second.getInferredClassName()) : cmp;
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

    public class NoteClassEntry implements Comparable<NoteClassEntry> {
        private final Class<? extends NoteClass> inferredClass;
        private int outScore;
        private int inScore;
        private int futureInScore;
        private NoteCollectionMemory memory;
        private Collection<RdfizationCallback> callbacks;

        public NoteClassEntry(Class<? extends NoteClass> inferredClass, int outScore, NoteCollectionMemory memory) {
            this.inferredClass = inferredClass;
            this.outScore = outScore;
            this.memory = memory;

            // note explicitly that in-score and future in-score (the working score) start at 0 for each entry
            inScore = 0;
            futureInScore = 0;
        }

        public Class<? extends NoteClass> getInferredClass() {
            return inferredClass;
        }

        public String getInferredClassName() {
            return classes.get(inferredClass).name;
        }

        public int getOutScore() {
            return outScore;
        }

        // compare based on out-score alone.  Used for the first stage of classification
        public int compareTo(NoteClassEntry other) {
            return ((Integer) outScore).compareTo(other.outScore);
        }

        public int getInScore() {
            return inScore;
        }

        /**
         * Returns the total score of this entry
         *
         * @return the total score of this entry, which is the sum of its out-score and in-score
         */
        public int getScore() {
            return inScore + outScore;
        }

        /**
         * A classification which has a score of zero is a trivial property-based regex match with no
         * structural evidence to support it.
         * These are not included in the exported RDF, as they include many false positives.
         * @return whether this classification has a nonzero confidence score
         */
        public boolean isNonTrivial() {
            return getScore() > 0;
        }
    }
}
