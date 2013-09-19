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

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class KnowledgeBase {
    private static final Logger LOGGER = Extendo.getLogger(KnowledgeBase.class);

    private final BrainGraph graph;
    private final BottomUpVocabulary vocabulary;

    private final Map<Atom, BottomUpType> typeOfAtom;

    public KnowledgeBase(final BrainGraph graph) {
        this.graph = graph;
        this.vocabulary = new BottomUpVocabulary();
        this.typeOfAtom = new HashMap<Atom, BottomUpType>();
    }

    public BottomUpVocabulary getVocabulary() {
        return vocabulary;
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
        // Account
        // ManufacturedPart
        // Place
        // SoftwareProject
    }

    public void matchSimpleTypes() {
        long total = 0;
        long mapped = 0;

        for (Atom a : graph.getAtoms()) {
            total++;
            if (null == typeOfAtom.get(a)) {
                BottomUpType t = firstMatchingSimpleType(a);

                if (null == t) {
                    //System.out.println("" + BrainGraph.getId(a) + "\t" + a.getValue());
                } else {
                    //System.out.println(t.getClass().getSimpleName() + "\t" + BrainGraph.getId(a) + "\t" + a.getValue());
                    typeOfAtom.put(a, t);
                    mapped++;
                }
            }
        }

        LOGGER.info("" + mapped + " of " + total + " atoms mapped to simple types");
    }

    public void matchCompoundTypes() {
        long total = 0;
        long mapped = 0;

        for (Atom a : graph.getAtoms()) {
            total++;

            if (null == typeOfAtom.get(a)) {
                BottomUpType t = firstMatchingCompoundType(a);
                if (null != t) {
                    System.out.println(t.getClass().getSimpleName() + "\t" + BrainGraph.getId(a) + "\t" + a.getValue());
                    typeOfAtom.put(a, t);
                    mapped++;
                }
            }
        }

        LOGGER.info("" + mapped + " of " + total + " atoms mapped to compound types");
    }

    // note: we assume for now that there is no overlap among simple types
    // i.e. that there is no atom which will be matched by more than one simple type
    public BottomUpType firstMatchingSimpleType(final Atom a) {
        String value = valueOf(a);

        // TODO: in future, perhaps an optimized matcher (by regex) can be written, so that each type does not need to be tested in series
        for (BottomUpType t : vocabulary.getSimpleTypes()) {
            if (simpleConstraintsSatisfied(a, value, t)) {
                return t;
            }
        }

        return null;
    }

    public BottomUpType firstMatchingCompoundType(final Atom a) {
        for (BottomUpType t : vocabulary.getCompoundTypes()) {
            if (compoundTypeMatches(a, t)) {
                return t;
            }
        }

        return null;
    }

    private boolean compoundTypeMatches(final Atom a,
                                        final BottomUpType type) {
        if (!simpleConstraintsSatisfied(a, a.getValue(), type)) {
            return false;
        }

        AtomList cur = a.getNotes();
        int fieldIndex = 0;
        Field[] fields = type.getFields();
        long matchingUniqueFields = 0;
        while (cur != null && fieldIndex < fields.length) {
            if (fieldMatches(cur.getFirst(), fields[fieldIndex])) {
                // this field matches, but it may or may not be a field which uniquely identifies the type
                if (fields[fieldIndex].getIsUnique()) {
                    matchingUniqueFields++;
                }
                cur = cur.getRest();
            }
            fieldIndex++;
        }

        // for now, if any *unique* fields match, it's an overall match
        return matchingUniqueFields > 0;
    }

    private boolean fieldMatches(final Atom a,
                                 final Field field) {
        // type constraint
        BottomUpType expectedType = field.getDataType();
        if (null != expectedType) {
            BottomUpType t = typeOfAtom.get(a);
            if (null == t || t != expectedType) {
                return false;
            }
        }

        BottomUpType containedType = field.getContainedDataType();
        if (null != containedType) {
            AtomList cur = a.getNotes();

            // If any child has a type other than the contained data type, this constraint is not satisfied.
            // However, not-yet-typed children are simply ignored until they are mapped.
            while (null != cur) {
                Atom child = cur.getFirst();
                BottomUpType t = typeOfAtom.get(child);
                if (null == t) {
                    // do nothing until mapping...
                } else if (t != containedType) {
                    return false;
                }
                cur = cur.getRest();
            }
        }

        String value = valueOf(a);

        // value regex constraint
        if (null != field.getValueRegex() && !field.getValueRegex().matcher(value).matches()) {
            return false;
        }

        // TODO: value additional constraints for fields?

        return true;
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
}
