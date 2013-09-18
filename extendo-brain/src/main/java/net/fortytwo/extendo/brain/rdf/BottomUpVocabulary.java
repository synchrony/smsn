package net.fortytwo.extendo.brain.rdf;

import net.fortytwo.extendo.Extendo;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
// TODO: optimize order of types in collection after all have been added
// or... leave it up to the user to add them in the right order, perhaps based on frequency of occurrence in the knowledge base
public class BottomUpVocabulary {
    private static final Logger LOGGER = Extendo.getLogger(BottomUpVocabulary.class);

    private final List<BottomUpType> allTypes;

    // types with no fields, i.e. which can be recognized without applying a regex to the atom's children
    private final List<BottomUpType> simpleTypes;
    // types with fields
    private final List<BottomUpType> compoundTypes;

    public BottomUpVocabulary() {
        allTypes = new LinkedList<BottomUpType>();
        simpleTypes = new LinkedList<BottomUpType>();
        compoundTypes = new LinkedList<BottomUpType>();
    }

    public void add(final BottomUpType t) {
        allTypes.add(t);

        if (0 == t.getFields().length) {
            simpleTypes.add(t);
        } else {
            compoundTypes.add(t);
        }
    }

    public List<BottomUpType> getSimpleTypes() {
        return simpleTypes;
    }

    public List<BottomUpType> getCompoundTypes() {
        return compoundTypes;
    }
}
