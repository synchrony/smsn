package net.fortytwo.extendo.brain.rdf;

import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.AtomList;
import net.fortytwo.extendo.brain.BrainGraph;
import net.fortytwo.extendo.brain.rdf.types.AKA;
import net.fortytwo.extendo.brain.rdf.types.ArticleOrBook;
import net.fortytwo.extendo.brain.rdf.types.BibtexReference;
import net.fortytwo.extendo.brain.rdf.types.Date;
import net.fortytwo.extendo.brain.rdf.types.ISBN;
import net.fortytwo.extendo.brain.rdf.types.OpenCollection;
import net.fortytwo.extendo.brain.rdf.types.Person;
import net.fortytwo.extendo.brain.rdf.types.RFID;
import net.fortytwo.extendo.brain.rdf.types.TODO;
import net.fortytwo.extendo.brain.rdf.types.TimeStampedEvent;
import net.fortytwo.extendo.brain.rdf.types.URL;
import net.fortytwo.extendo.brain.rdf.types.VocabularyTerm;
import net.fortytwo.extendo.brain.rdf.types.WebPage;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
/*
typical steps in the mapping process:
1) identify simple types
2) identify compound types
3) return to (2) until no more compound types are found
4) coerce not-yet-typed container members
5) map to RDF
 */
public class KnowledgeBase {
    private static final Logger LOGGER = Extendo.getLogger(KnowledgeBase.class);

    private final BrainGraph graph;
    private final BottomUpVocabulary vocabulary;

    // the type of an atom as determined by its properties and the local tree of which it is the root
    private final Map<Atom, BottomUpType> typeOfAtom;
    // the type of an atom as determined by local trees in which it is not the root
    private final Map<Atom, BottomUpType> inferredTypeOfAtom;

    public static boolean isCollectionType(final BottomUpType type) {
        // TODO: generalize this
        return type == OpenCollection.INSTANCE;
    }

    public KnowledgeBase(final BrainGraph graph) {
        this.graph = graph;
        this.vocabulary = new BottomUpVocabulary();
        this.typeOfAtom = new HashMap<Atom, BottomUpType>();
        this.inferredTypeOfAtom = new HashMap<Atom, BottomUpType>();
    }

    public BottomUpVocabulary getVocabulary() {
        return vocabulary;
    }

    public BottomUpType getTypeOf(final Atom a) {
        return typeOfAtom.get(a);
    }

    private void setTypeOf(final Atom a,
                           final BottomUpType type) {
        typeOfAtom.put(a, type);
    }

    public BottomUpType getInferredTypeOf(final Atom a) {
        return inferredTypeOfAtom.get(a);
    }

    /**
     * Provisionally associates an inferred type with an atom.
     * If, in a subsequent phase, the atom is found to be valid with respect to the type,
     * it will be mapped to RDF as an instance of that type.
     */
    public void setInferredTypeOf(final Atom a,
                                  final BottomUpType type) {
        BottomUpType existingType = inferredTypeOfAtom.get(a);
        if (null != existingType && existingType != type) {
            LOGGER.warning("conflicting inferred types for atom " + a.asVertex().getId() + ": {"
                    + existingType.getClass().getSimpleName() + ", " + type.getClass().getSimpleName() + "}. Only the former will be tried.");
        } else {
            inferredTypeOfAtom.put(a, type);
        }
    }

    public void addDefaultTypes() {
        BottomUpVocabulary v = vocabulary;

        v.add(AKA.INSTANCE);
        v.add(BibtexReference.INSTANCE);
        v.add(Date.INSTANCE);
        v.add(ISBN.INSTANCE);
        v.add(RFID.INSTANCE);
        v.add(TODO.INSTANCE);
        v.add(URL.INSTANCE);
        v.add(VocabularyTerm.INSTANCE);
        v.add(WebPage.INSTANCE);

        // types with the most inclusive regex are last
        v.add(ArticleOrBook.INSTANCE);
        v.add(OpenCollection.INSTANCE);
        v.add(Person.INSTANCE);
        v.add(TimeStampedEvent.INSTANCE);

        // TODO
        // * Account
        // * ManufacturedPart
        // * Place
        // * SoftwareProject
    }

    public void matchSimpleTypes() {
        long total = 0;
        long mapped = 0;

        for (Atom a : graph.getAtoms()) {
            total++;
            if (null == getTypeOf(a)) {
                BottomUpType t = firstMatchingSimpleType(a);

                if (null == t) {
                    //System.out.println("" + BrainGraph.getId(a) + "\t" + a.getValue());
                } else {
                    //System.out.println(t.getClass().getSimpleName() + "\t" + BrainGraph.getId(a) + "\t" + a.getValue());
                    setTypeOf(a, t);
                    mapped++;
                }
            }
        }

        LOGGER.info("" + mapped + " of " + total + " atoms mapped to simple types");
    }

    public void matchCompoundTypes() throws RDFHandlerException {
        long total;
        long mapped;
        int pass = 1;

        do {
            total = 0;
            mapped = 0;

            for (Atom a : graph.getAtoms()) {
                total++;

                if (null == getTypeOf(a)) {
                    BottomUpType t = firstMatchingCompoundType(a);
                    if (null != t) {
                        //System.out.println(t.getClass().getSimpleName() + "\t" + BrainGraph.getId(a) + "\t" + a.getValue());
                        setTypeOf(a, t);
                        mapped++;
                    }
                }
            }

            LOGGER.info("pass #" + pass + ": " + mapped + " of " + total + " atoms mapped to compound types");
            pass++;
        } while (mapped > 0);
    }

    public void coerceUntypedAtoms() throws RDFHandlerException {
        long inferred = 0;
        long coerced = 0;

        for (Atom a : graph.getAtoms()) {
            BottomUpType it = getInferredTypeOf(a);
            if (null != it) {
                inferred++;
                BottomUpType t = getTypeOf(a);
                if (null == t) {
                    LOGGER.info("attempting to coerce to an inferred type");
                    if (matchCompoundType(a, it, null, false)) {
                        // from this point on, the inferred type is also the actual type
                        setTypeOf(a, it);
                        coerced++;
                    }
                }
            }
        }

        LOGGER.info("" + coerced + " of " + inferred + " untyped atoms coerced to their inferred types");
    }

    public void generateRDF(final RDFHandler handler,
                            final ValueFactory vf) throws RDFHandlerException {

        LOGGER.info("generating RDF data from knowledge base");
        LOGGER.warning("no sharability restrictions have been enforced");

        handler.startRDF();

        MappingContext mc = new MappingContext(this);
        mc.setHandler(handler);
        mc.setValueFactory(vf);

        for (Atom a : graph.getAtoms()) {
            BottomUpType t = getTypeOf(a);
            if (null != t) {
                mc.setReference(a);
                mc.setReferenceUri(vf.createURI(BrainGraph.uriOf(a)));
                //t.translateToRDF(a, vf, handler);
                matchCompoundType(a, t, mc, false);
            }
        }

        handler.endRDF();
    }

    // note: we assume for now that there is no overlap among simple types
    // i.e. that there is no atom which will be matched by more than one simple type
    private BottomUpType firstMatchingSimpleType(final Atom a) {
        String value = valueOf(a);

        // TODO: in future, perhaps an optimized matcher (by regex) can be written, so that each type does not need to be tested in series
        for (BottomUpType t : vocabulary.getSimpleTypes()) {
            if (simpleConstraintsSatisfied(a, value, t)) {
                return t;
            }
        }

        return null;
    }

    private BottomUpType firstMatchingCompoundType(final Atom a) throws RDFHandlerException {
        for (BottomUpType t : vocabulary.getCompoundTypes()) {
            // strict checking is required here; we are identifying instances based on their properties and
            // children alone, as opposed to their inbound connections
            if (matchCompoundType(a, t, null, true)) {
                return t;
            }
        }

        return null;
    }

    /**
     * @param a      an atom to match against
     * @param type   a type to match
     * @param mc     a context for mapping matched fields to RDF.  If null, no mapping is performed
     * @param strict whether this atom requires strict matching (i.e. can't be accepted as an instance of the type
     *               unless it contains some uniquely-qualifying fields, in addition to being syntactically valid)
     * @return whether the type successfully matched
     */
    private boolean matchCompoundType(final Atom a,
                                      final BottomUpType type,
                                      final MappingContext mc,
                                      final boolean strict) throws RDFHandlerException {
        RDFBuffer buffer = null == mc ? null : new RDFBuffer(mc.getHandler());

        if (!simpleConstraintsSatisfied(a, a.getValue(), type)) {
            return false;
        }

        AtomList cur = a.getNotes();
        int fieldIndex = 0;
        Field[] fields = type.getFields();
        long matchingUniqueFields = 0;
        while (cur != null && fieldIndex < fields.length) {
            Atom fa = cur.getFirst();
            Field f = fields[fieldIndex];
            if (matchField(fa, f)) {
                if (null != mc) {
                    mapField(fa, f, mc);
                }

                // this field matches, but it may or may not be a field which uniquely identifies the type
                if (fields[fieldIndex].getIsUnique()) {
                    matchingUniqueFields++;
                }
                cur = cur.getRest();
            }
            fieldIndex++;
        }

        // for now, it's an overall match if and only if any *unique* fields match
        boolean matched = !strict || matchingUniqueFields > 0;

        if (matched && null != mc) {
            type.translateToRDF(a, mc.getValueFactory(), buffer);

            buffer.flush();
        }

        return matched;
    }

    private boolean matchField(final Atom fa,
                               final Field field) {
        // type constraint
        BottomUpType expectedType = field.getDataType();
        if (null != expectedType) {
            BottomUpType t = getTypeOf(fa);
            if (null == t || t != expectedType) {
                return false;
            }
        }

        String value = valueOf(fa);

        // value regex constraint
        if (null != field.getValueRegex() && !field.getValueRegex().matcher(value).matches()) {
            return false;
        }

        // TODO: additional value constraints for fields?

        // Note: for containers, we do not demand that the children of the
        // container are of the expected type.  Instead, we use the
        // expected type to coerce any still-untyped children when mapping to
        // RDF.
        // Badly-typed children of containers are simply tolerated and ignored.
        // If, on the contrary, a strict type check for children were enforced
        // here, many otherwise recognizable instances of types would be
        // excluded on the basis of a single list item which does not conform.
        // For example, if Confucius is primarily identified by as a person by
        // his long list of quotations, and one of those quotations is not
        // properly enclosed in quotation marks, it shouldn't cause Confucius to
        // disappear entirely from the mapping; only that invalid quote will be
        // missing.
        if (isCollectionType(field.getDataType())) {

            BottomUpType type = field.getContainedDataType();
            if (null != type) {
                for (Atom child : contentsOfCollection(fa)) {
                    BottomUpType t = getTypeOf(child);

                    // If any child has a type other than the contained data type, the child is ignored.
                    // However, not-yet-typed children are to be coerced if they are valid instances of the type.
                    if (null == t) {
                        setInferredTypeOf(child, type);
                    }
                }
            }
        }

        return true;
    }

    private void mapField(final Atom a,
                          final Field field,
                          final MappingContext mc) throws RDFHandlerException {
        field.getMapper().mapToRDF(a, mc);
    }

    private boolean simpleConstraintsSatisfied(final Atom a,
                                               final String value,
                                               final BottomUpType t) {
        // value regex constraint
        if (null != t.getValueRegex() && !t.getValueRegex().matcher(value).matches()) {
            return false;
        }

        // value additional constraints
        if (!t.additionalConstraintsSatisfied(value)) {
            return false;
        }

        // alias constraint
        if (t.aliasRequired() && null == a.getAlias()) {
            return false;
        }

        // children constraint
        if (t.childrenRequired() && null == a.getNotes()) {
            return false;
        }

        return true;
    }

    private String valueOf(final Atom a) {
        String value = a.getValue();
        if (null == value) {
            LOGGER.warning("atom with id '" + BrainGraph.getId(a) + "' has null value");
        }

        return value;
    }

    // note: the top-level atom is assumed to be a collection
    public Collection<Atom> contentsOfCollection(final Atom collection) {
        Collection<Atom> result = new LinkedList<Atom>();
        contentsOfCollectionRecursive(collection, result);
        return result;
    }

    private void contentsOfCollectionRecursive(final Atom a,
                                               final Collection<Atom> result) {
        BottomUpType t = getTypeOf(a);
        if (null == t || !isCollectionType(t)) {
            result.add(a);
        } else {
            AtomList cur = a.getNotes();

            while (null != cur) {
                Atom child = cur.getFirst();
                contentsOfCollectionRecursive(child, result);

                cur = cur.getRest();
            }
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
}
