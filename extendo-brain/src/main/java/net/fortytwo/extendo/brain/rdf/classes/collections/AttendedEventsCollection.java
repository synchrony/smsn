package net.fortytwo.extendo.brain.rdf.classes.collections;

import net.fortytwo.extendo.brain.rdf.AtomCollection;
import net.fortytwo.extendo.brain.rdf.AtomRegex;
import net.fortytwo.extendo.brain.rdf.classes.AbstractEvent;
import net.fortytwo.extendo.brain.rdf.classes.Document;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class AttendedEventsCollection extends AtomCollection {

    public AttendedEventsCollection() {
        super(
                "attended-events-collection",
                Pattern.compile("(some (.+ )?events .+ attended)"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.OneOrMore, AbstractEvent.class, AttendedEventsCollection.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore)
                )));
    }
}
