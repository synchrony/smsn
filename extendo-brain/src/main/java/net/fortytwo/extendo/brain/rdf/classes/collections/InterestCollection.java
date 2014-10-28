package net.fortytwo.extendo.brain.rdf.classes.collections;

import net.fortytwo.extendo.brain.rdf.AtomCollection;
import net.fortytwo.extendo.brain.rdf.AtomRegex;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class InterestCollection extends AtomCollection {
    public static final InterestCollection INSTANCE = new InterestCollection();

    public InterestCollection() {
        super(
                "interests-of",
                Pattern.compile("some things .+ like[sd]?"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore, GenericCollection.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore)
                )));
    }
}
