package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.rdf.SimpleAtomClass;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Date extends SimpleAtomClass {

    public Date() {
        super(
                "date",
                Pattern.compile("[12][0-9][0-9][0-9]-[012][0-9]-[0123][0-9]"),
                null,
                null
        );
    }
}
