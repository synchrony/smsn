package net.fortytwo.extendo.brain.rdf;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.AtomList;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class AtomMatcher {
    // TODO: generate RDF as you go, and discard the generated RDF if/when the atom is found *not* to match
    public boolean match(final BottomUpType type,
                         final Atom atom) {
        if (type.aliasRequired()) {
            if (null == atom.getAlias()) {
                return false;
            }
        }

        if (type.childrenRequired()) {
            if (null == atom.getNotes()) {
                return false;
            }
        }

        if (null != type.getValueRegex()) {
            if (!type.getValueRegex().matcher(atom.getValue()).matches()) {
                return false;
            }
        }

        if (null != type.getFields() && 0 < type.getFields().length) {
            AtomList aCur = atom.getNotes();
            int i = 0;

            // TODO
            // ...

            return true;
        } else {
            return true;
        }
    }
}
