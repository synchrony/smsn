package net.fortytwo.extendo.brain.rdf.classes.collections;

import net.fortytwo.extendo.brain.rdf.AtomCollection;
import net.fortytwo.extendo.brain.rdf.AtomRegex;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GenericCollection extends AtomCollection {
    public static final GenericCollection INSTANCE = new GenericCollection();

    public GenericCollection() {
        super(
                "collection",
                Pattern.compile("some .+"),
                null,
                new AtomRegex(Arrays.asList(
                        // note: this permits empty collections, matching values which simply begin with "some "
                        // However, the scoring system will penalize such a match unless there is other evidence
                        // that this is a collection (e.g. it is found at the head of other collections).
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore, GenericCollection.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore)
                )));
    }
}
