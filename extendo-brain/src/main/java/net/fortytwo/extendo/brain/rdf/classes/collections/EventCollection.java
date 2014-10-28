package net.fortytwo.extendo.brain.rdf.classes.collections;

import net.fortytwo.extendo.brain.rdf.AtomCollection;
import net.fortytwo.extendo.brain.rdf.AtomRegex;
import net.fortytwo.extendo.brain.rdf.classes.DatedEvent;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class EventCollection extends AtomCollection {
    public static final EventCollection INSTANCE = new EventCollection();

    public EventCollection() {
        super(
                "event-collection",
                Pattern.compile("some .+"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.OneOrMore, DatedEvent.class, EventCollection.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore)
                )));
    }
}
