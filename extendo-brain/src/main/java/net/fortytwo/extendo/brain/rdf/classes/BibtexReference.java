package net.fortytwo.extendo.brain.rdf.classes;

import net.fortytwo.extendo.brain.rdf.SimpleAtomClass;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BibtexReference extends SimpleAtomClass {

    public BibtexReference() {
        super(
                "bibtex-reference",
                Pattern.compile("bibtex: (.|\\s)+"),
                null,
                null
        );
    }
}
