package net.fortytwo.smsn.brain.rdf.classes.collections;

import net.fortytwo.smsn.brain.rdf.NoteCollection;
import net.fortytwo.smsn.brain.rdf.NoteReqex;
import net.fortytwo.smsn.brain.rdf.classes.AbstractEvent;

import java.util.Arrays;
import java.util.regex.Pattern;

public class AttendedEventsCollection extends NoteCollection {

    public AttendedEventsCollection() {
        super(
                "attended-events-collection",
                Pattern.compile("(some (.+ )?events .+ attended)"),
                null,
                new NoteReqex(Arrays.asList(
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.OneOrMore, AbstractEvent.class, AttendedEventsCollection.class),
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore)
                )));
    }
}
