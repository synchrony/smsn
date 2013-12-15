package net.fortytwo.extendo.brain.rdf;

import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.AtomList;
import net.fortytwo.extendo.brain.BrainGraph;
import net.fortytwo.extendo.brain.rdf.types.AKA;
import net.fortytwo.extendo.brain.rdf.types.BibtexReference;
import net.fortytwo.extendo.brain.rdf.types.Date;
import net.fortytwo.extendo.brain.rdf.types.Document;
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
import java.util.HashSet;
import java.util.LinkedList;
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
    private static final Logger LOGGER = Extendo.getLogger(KnowledgeBase.class);

    private final BrainGraph graph;
    private final BottomUpVocabulary vocabulary;

    // the type of an atom as determined by its properties and the local tree of which it is the root
    private final Map<Atom, BottomUpType> typeOfAtom;
    // the type of an atom as determined by local trees in which it is not the root
    private final Map<Atom, BottomUpType> inferredTypeOfAtom;

    private final Set<BottomUpType> simpleTypes = new HashSet<BottomUpType>();

    private boolean changed;

    public static boolean isCollectionType(final BottomUpType type) {
        return type instanceof OpenCollection;
    }

    public KnowledgeBase(final BrainGraph graph) {
        this.graph = graph;
        this.vocabulary = new BottomUpVocabulary();
        this.typeOfAtom = new HashMap<Atom, BottomUpType>();
        this.inferredTypeOfAtom = new HashMap<Atom, BottomUpType>();
    }

    // note: graph and vocabulary are not affected by this operation
    public void reset() {
        typeOfAtom.clear();
        inferredTypeOfAtom.clear();
    }

    public BottomUpType getTypeOf(final Atom a) {
        return typeOfAtom.get(a);
    }

    private void setTypeOf(final Atom a,
                           final BottomUpType type) {
        BottomUpType existingType = typeOfAtom.get(a);
        if (null == existingType) {
            typeOfAtom.put(a, type);
            changed = true;
        } else if (existingType != type) {
            LOGGER.warning("conflicting types for atom " + a.asVertex().getId() + ": {"
                    + existingType.getName() + ", " + type.getName() + "}. Only the former will be used.");
        }
    }

    public BottomUpType getInferredTypeOf(final Atom a) {
        return inferredTypeOfAtom.get(a);
    }

    /**
     * Provisionally associates an inferred type with an atom.
     * If, in a subsequent phase, the atom is found to be valid with respect to the type,
     * it will be mapped to RDF as an instance of that type.
     */
    private void setInferredTypeOf(final Atom a,
                                   final BottomUpType type) {
        BottomUpType existingType = inferredTypeOfAtom.get(a);
        if (null == existingType) {
            inferredTypeOfAtom.put(a, type);
            changed = true;
        } else if (existingType != type) {
            LOGGER.warning("conflicting inferred types for atom " + a.asVertex().getId() + ": {"
                    + existingType.getName() + ", " + type.getName() + "}. Only the former will be used.");
        }
    }

    private long setInferredTypeOfMembers(final Atom container,
                                          final OpenCollection type) {
        long count = 0;

        BottomUpType containedType = type.getContainedType();
        if (null != containedType) {

            // TODO: temporary debugging code; remove
            //if (container.asVertex().getId().equals("UG&hPK4")) {
            //    System.out.println("typing members of " + container.asVertex().getId() + " as " + type.getContainedType().getName());
            //    new Exception().printStackTrace(System.out);
            //}

            for (Atom child : contentsOfCollection(container)) {
                BottomUpType t = getTypeOf(child);

                // TODO: temporary debugging code; remove
                //if (container.asVertex().getId().equals("UG&hPK4")) {
                //    System.out.println("\t" + t + " " + child.getValue());
                //}

                // If any child has a type other than the contained data type, the child is ignored.
                // However, not-yet-typed children are to be coerced if they are valid instances of the type.
                if (null == t) {
                    if (simpleConstraintsSatisfied(container, child.getValue(), containedType, false)) {

                        // TODO: temporary debugging code; remove
                        //if (container.asVertex().getId().equals("UG&hPK4")) {
                        //    System.out.println("\t\tsetting inferred type of " + child.getValue() + " to " + containedType);
                        //}

                        setInferredTypeOf(child, containedType);
                        count++;
                    }
                }
            }
        }

        return count;
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
        v.add(Document.INSTANCE);
        v.add(OpenCollection.GENERIC_INSTANCE);
        v.add(Person.INSTANCE);
        v.add(TimeStampedEvent.INSTANCE);

        // TODO
        // * Account
        // * ManufacturedPart
        // * Place
        // * SoftwareProject

        simpleTypes.clear();
        simpleTypes.addAll(v.getSimpleTypes());
    }

    private boolean isSimpleType(final BottomUpType type) {
        return simpleTypes.contains(type);
    }

    public void inferTypes() throws RDFHandlerException {
        changed = false;
        matchSimpleTypes();
        matchCompoundTypes();

        long typed = typeOfAtom.size();
        long total = countAtoms();
        LOGGER.info("" + typed + " of " + total + " atoms have been typed (" + (total - typed) + " remaining)");
    }

    private long countAtoms() {
        long count = 0;
        for (Atom a : graph.getAtoms()) {
            count++;
        }
        return count;
    }

    private void matchSimpleTypes() {
        long total = 0;
        long mapped = 0;

        for (Atom atom : graph.getAtoms()) {
            if (null == getTypeOf(atom)) {
                total++;
                BottomUpType t = firstMatchingSimpleType(atom);

                if (null == t) {
                    //System.out.println("" + BrainGraph.getId(a) + "\t" + a.getValue());
                } else {
                    // TODO: temporary debugging code; remove
                    //if (atom.asVertex().getId().equals("KfiWlJj")) {
                    //    System.out.println("recognized atom " + atom.asVertex().getId() + " as simple type " + t.getName());
                    //}

                    //System.out.println(t.getClass().getSimpleName() + "\t" + BrainGraph.getId(a) + "\t" + a.getValue());
                    setTypeOf(atom, t);
                    mapped++;
                }
            }
        }

        LOGGER.info("" + mapped + " of " + total + " untyped atoms mapped to simple types");
    }

    private void matchCompoundTypes() throws RDFHandlerException {
        long total;
        long mapped, inferred, coerced;
        int pass = 1;

        do {
            changed = false;

            total = 0;
            mapped = 0;
            coerced = 0;
            inferred = 0;

            for (Atom atom : graph.getAtoms()) {
                BottomUpType type = getTypeOf(atom);
                if (null == type || !isSimpleType(type)) {
                //if (null == getTypeOf(atom)) {
                    total++;

                    BottomUpType t = firstMatchingCompoundType(atom);
                    if (null != t) {
                        // TODO: temporary debugging code; remove
                        //if (atom.asVertex().getId().equals("KfiWlJj")) {
                        //    System.out.println("recognized " + atom.getValue() + " as compound type " + t.getName());
                        //}

                        //System.out.println(t.getClass().getSimpleName() + "\t" + BrainGraph.getId(a) + "\t" + a.getValue());
                        setTypeOf(atom, t);
                        mapped++;

                        if (isCollectionType(t)) {
                            inferred += setInferredTypeOfMembers(atom, (OpenCollection) t);
                        }
                    }
                }
            }

            LOGGER.info("pass #" + pass + ": " + mapped + " of " + total + " untyped atoms mapped to compound types");

            coerceUntypedAtoms();

            pass++;

            /*
            // TODO: temporary extra pass
            for (Atom a : graph.getAtoms()) {
                BottomUpType t = getTypeOf(a);
                if (null != t) {
                    matchCompoundType(a, t, null, false);
                }
            }//*/
        } while (changed);
    }

    private long coerceUntypedAtoms() throws RDFHandlerException {
        long inferred = 0;
        long coerced = 0;

        for (Atom atom : graph.getAtoms()) {

            // TODO: temporary debugging code; remove
            //if (atom.asVertex().getId().equals("SBZFumn")) {
            //    System.out.println("inspecting " + atom.getValue() + " for coercion");
            //}

            BottomUpType it = getInferredTypeOf(atom);
            if (null != it) {
                inferred++;
                BottomUpType t = getTypeOf(atom);
                if (null == t) {
                    // non-strict matching; simple constraints have already been applied once
                    if (matchCompoundType(atom, it, null, false)) {
                        // TODO: temporary debugging code; remove
                        //if (atom.asVertex().getId().equals("KfiWlJj")) {
                        //    System.out.println("coerced atom " + atom.asVertex().getId() + " to compound type " + it.getName());
                        //}

                        // from this point on, the inferred type is also the actual type
                        setTypeOf(atom, it);
                        coerced++;
                    }
                }
            }
        }

        LOGGER.info("" + coerced + " of " + inferred + " untyped atoms coerced to their inferred types");

        return coerced;
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
            if (simpleConstraintsSatisfied(a, value, t, false)) {
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
     * @param atom   an atom to match against
     * @param type   a type to match
     * @param mc     a context for mapping matched fields to RDF.  If null, no mapping is performed
     * @param strict whether this atom requires strict matching (i.e. can't be accepted as an instance of the type
     *               unless it contains some uniquely-qualifying fields, in addition to being syntactically valid)
     * @return whether the type successfully matched
     */
    private boolean matchCompoundType(final Atom atom,
                                      final BottomUpType type,
                                      final MappingContext mc,
                                      final boolean strict) throws RDFHandlerException {
        // TODO: temporary debugging code; remove
        //if (atom.asVertex().getId().equals("KfiWlJj")) {
        //    System.out.println("matching compound type " + type.getName() + " for " + atom.getValue());
        //}

        RDFBuffer buffer = null == mc ? null : new RDFBuffer(mc.getHandler());

        if (!simpleConstraintsSatisfied(atom, atom.getValue(), type, !strict)) {
            return false;
        }

        boolean matched = !strict;

        AtomList cur = atom.getNotes();
        int fieldIndex = 0;
        Field[] fields = type.getFields();
        while (cur != null && fieldIndex < fields.length) {
            Atom fa = cur.getFirst();
            Field f = fields[fieldIndex];
            if (matchField(fa, f)) {
                // if any fields are found which are distinct to the candidate type, it is a match
                if (fields[fieldIndex].getIsDistinctive()) {
                    matched = true;
                }

                if (null != mc) {
                    mapField(fa, f, mc);
                }

                cur = cur.getRest();
            }
            fieldIndex++;
        }

        if (matched && null != mc) {
            type.translateToRDF(atom, mc.getValueFactory(), buffer);

            buffer.flush();
        }

        return matched;
    }

    private boolean matchField(final Atom atom,
                               final Field field) {
        // TODO: temporary debugging code; remove
        //if (atom.asVertex().getId().equals("UG&hPK4")) {
        //    System.out.println("matching field " + field.getValueRegex().pattern() + " against atom " + atom.asVertex().getId());
        //}

        String value = valueOf(atom);

        // field-specific value regex constraint
        if (null != field.getValueRegex() && !field.getValueRegex().matcher(value).matches()) {
            return false;
        }

        // TODO: temporary debugging code; remove
        //if (atom.asVertex().getId().equals("UG&hPK4")) {
        //    System.out.println("a");
        //}

        // TODO: additional value constraints for fields?

        // type constraint
        // note: expected type is never null
        BottomUpType expectedType = field.getDataType();
        BottomUpType actualType = getTypeOf(atom);
        if (null == actualType && null != field.getValueRegex()) {

            // TODO: temporary debugging code; remove
            //if (atom.asVertex().getId().equals("UG&hPK4")) {
            //    System.out.println("testing " + atom.asVertex().getId());
            //}

            if (simpleConstraintsSatisfied(atom, value, expectedType, true)) {
                setInferredTypeOf(atom, expectedType);

                // TODO: temporary debugging code; remove
                //if (atom.asVertex().getId().equals("UG&hPK4")) {
                //    System.out.println("\tinferred type as " + expectedType.getName());
                //}

                return true;
            } else {
                return false;
            }
        } else if (actualType != expectedType) {
            return false;
        }

        // TODO: temporary debugging code; remove
        //if (atom.asVertex().getId().equals("UG&hPK4")) {
        //    System.out.println("s");
        //}

        /*
        For containers, we do not demand that the children of the
        container are of the expected type.  Instead, we use the
        expected type to coerce any still-untyped children when mapping to
        RDF.
        Badly-typed children of containers are simply tolerated and ignored.
        If, on the contrary, a strict type check for children were enforced
        here, many otherwise recognizable instances of types would be
        excluded on the basis of a single list item which does not conform.
        For example, if Confucius is primarily identified by as a person by
        his long list of quotations, and one of those quotations is not
        properly enclosed in quotation marks, it shouldn't cause Confucius to
        disappear entirely from the mapping; only that invalid quote will be
        missing.
        */
        if (isCollectionType(field.getDataType())) {
            setInferredTypeOfMembers(atom, (OpenCollection) field.getDataType());
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
                                               final BottomUpType t,
                                               final boolean ignoreRegex) {
        if (!ignoreRegex) {
            // value regex constraint
            if (null != t.getValueRegex() && !t.getValueRegex().matcher(value).matches()) {
                return false;
            }
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
