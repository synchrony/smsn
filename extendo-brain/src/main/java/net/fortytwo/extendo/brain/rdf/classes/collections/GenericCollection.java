package net.fortytwo.extendo.brain.rdf.classes.collections;

import net.fortytwo.extendo.brain.rdf.AtomCollection;
import net.fortytwo.extendo.brain.rdf.AtomRegex;
import net.fortytwo.extendo.brain.rdf.classes.AbstractEvent;
import net.fortytwo.extendo.brain.rdf.classes.DatedEvent;
import net.fortytwo.extendo.brain.rdf.classes.Document;
import net.fortytwo.extendo.brain.rdf.classes.LinkedConcept;
import net.fortytwo.extendo.brain.rdf.classes.Person;
import net.fortytwo.extendo.brain.rdf.classes.QuotedValue;
import net.fortytwo.extendo.brain.rdf.classes.TODOTask;
import net.fortytwo.extendo.brain.rdf.classes.Tool;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GenericCollection extends AtomCollection {

    public GenericCollection() {
        super(
                "collection",
                Pattern.compile("((some|my|our|([A-Z][a-z- ].*[a-z]('s|s')))" +
                        // note: the below only recognizes plurals ending in -s,
                        // but you can only do so much with regex
                        "|the [A-Za-z][A-Za-z- ]*[a-z]s) .+"),
                null,
                new AtomRegex(Arrays.asList(
                        // note: this permits empty collections, matching values which simply begin with "some "
                        // However, the scoring system will penalize such a match unless there is other evidence
                        // that this is a collection (e.g. it is found at the head of other collections).
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore, GenericCollection.class),
//                        new AtomRegex.El(null,
//                                AtomRegex.Modifier.ZeroOrMore)

                        // listing specific classes, rather than allowing all classes,
                        // prevents the collection class from matching structures with simple fields
                        // However, we make the element generic so that a collection does not compete
                        // with classes which match a structure more specifically.
                        new AtomRegex.El(0, null,
                                AtomRegex.Modifier.ZeroOrMore,
                                AbstractEvent.class,
                                DatedEvent.class,
                                Document.class,
                                LinkedConcept.class,
                                Person.class,
                                QuotedValue.class,
                                TODOTask.class,
                                Tool.class)
                )));
    }
}
