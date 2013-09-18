package net.fortytwo.extendo.brain.rdf;

import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Atom;

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

    // types which can be recognized without applying a regex to the atom's children
    private final List<BottomUpType> simpleTypes;

    public BottomUpVocabulary() {
        simpleTypes = new LinkedList<BottomUpType>();
        allTypes = new LinkedList<BottomUpType>();
    }

    public void add(final BottomUpType t) {
        allTypes.add(t);

        if (0 == t.getFields().length) {
            simpleTypes.add(t);
        }
    }

    public BottomUpType findFirstSuperficialMatch(final Atom a) {
        String value = a.getValue();
        if (null == value) {
            LOGGER.warning("atom with id '" + a.asVertex().getId() + "' has null value");
            return null;
        }

        for (BottomUpType t : simpleTypes) {
            // value constraint
            if (null != t.getValueRegex() && !t.getValueRegex().matcher(value).matches()) {
                continue;
            }

            // alias constraint
            if (t.aliasRequired() && null == a.getAlias()) {
                continue;
            }

            // children constraint
            if (t.childrenRequired() && null == a.getNotes()) {
                continue;
            }

            return t;
        }

        return null;
    }
}
