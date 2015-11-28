package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.rdf.SimpleAtomClass;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BibtexReference extends SimpleAtomClass {

    public BibtexReference() {
        super(
                "bibtex-reference",
                Pattern.compile("\\\\cite\\{[^\\n\\r\\t \\\"#%'(){}\\\\]+\\}"),
                null,
                null
        );
    }
}
