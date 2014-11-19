package net.fortytwo.extendo.brain.rdf.classes;

import net.fortytwo.extendo.brain.rdf.AtomRegex;
import net.fortytwo.extendo.brain.rdf.SimpleAtomClass;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * A collection of how-to notes relating to a tool
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Usage extends SimpleAtomClass {

    public Usage() {
        super(
                "usage",
                Pattern.compile(".+ usage"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(null,
                                // usage implies some content, regardless of what it is
                                AtomRegex.Modifier.OneOrMore))));
    }
}
