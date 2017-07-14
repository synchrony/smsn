package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.rdf.AtomRegex;
import net.fortytwo.smsn.brain.rdf.SimpleAtomClass;

import java.util.Collections;
import java.util.regex.Pattern;

/**
 * A collection of how-to notes relating to a tool
 */
public class Usage extends SimpleAtomClass {

    public Usage() {
        super(
                "usage",
                Pattern.compile(".+ usage"),
                null,
                new AtomRegex(Collections.singletonList(
                        new AtomRegex.El(null,
                                // usage implies some content, regardless of what it is
                                AtomRegex.Modifier.OneOrMore))));
    }
}
