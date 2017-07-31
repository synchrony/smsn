package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.rdf.SimpleNoteClass;

import java.util.regex.Pattern;

public class BibtexReference extends SimpleNoteClass {

    public BibtexReference() {
        super(
                "bibtex-reference",
                Pattern.compile("\\\\cite\\{[^\\n\\r\\t \\\"#%'(){}\\\\]+\\}"),
                null,
                null
        );
    }
}
