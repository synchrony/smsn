package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.rdf.SimpleNoteClass;

import java.util.regex.Pattern;

public class Date extends SimpleNoteClass {

    public Date() {
        super(
                "date",
                Pattern.compile("[12][0-9][0-9][0-9]-[012][0-9]-[0123][0-9]"),
                null,
                null
        );
    }
}
