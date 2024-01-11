package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.rdf.SimpleNoteClass;

import java.util.regex.Pattern;

public class AKAReference extends SimpleNoteClass {

    public AKAReference() {
        super(
                "aka",
                // TODO: the foaf:nick mapping is not quite appropriate to the "brand/trade name" and "formerly" usage
                Pattern.compile("(aka|brand name|trade name|formerly) \\\"[^\\\"]+\\\"(, \\\"[^\\\"]+\\\")*"),
                null,
                null
                );
    }

    public static String extractAlias(final String value) {
        int i = value.indexOf('\"');
        int j = value.lastIndexOf('\"');
        return value.substring(i + 1, j);
    }
}
