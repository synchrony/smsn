package net.fortytwo.smsn.brain.rdf.classes.collections;

import net.fortytwo.smsn.brain.rdf.NoteCollection;
import net.fortytwo.smsn.brain.rdf.NoteReqex;

import java.util.Arrays;
import java.util.regex.Pattern;

public class GenericCollection extends NoteCollection {

    public GenericCollection() {
        super(
                "collection",
                Pattern.compile("((some|my|our|([A-Z][a-z- ].*[a-z]('s|s')))" +
                        // note: the below only recognizes plurals ending in -s,
                        // but you can only do so much with regex
                        "|the [A-Za-z][A-Za-z- ]*[a-z]s) .+"),
                null,
                new NoteReqex(Arrays.asList(
                        // note: this permits empty collections, matching values which simply begin with "some "
                        // However, the scoring system will penalize such a match unless there is other evidence
                        // that this is a collection (e.g. it is found at the head of other collections).
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore,
                                AttendedEventsCollection.class,
                                DocumentCollection.class,
                                GenericCollection.class,
                                PersonCollection.class,
                                QuotedValueCollection.class,
                                TODOCollection.class,
                                TopicCollection.class),
                        new NoteReqex.El(0, null,
                                NoteReqex.Modifier.ZeroOrMore)

                        /*
                        // listing specific classes, rather than allowing all classes,
                        // prevents the collection class from matching structures with simple fields
                        // However, we make the element generic so that a collection does not compete
                        // with classes which match a structure more specifically.
                        new NoteRegex.El(0, null,
                                NoteRegex.Modifier.ZeroOrMore,
                                AbstractEvent.class,
                                DatedEvent.class,
                                Document.class,
                                LinkedConcept.class,
                                Person.class,
                                QuotedValue.class,
                                TODOTask.class,
                                Tool.class)
                                */
                )));
    }
}
