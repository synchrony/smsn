package net.fortytwo.smsn.brain.rdf.classes.collections;

import net.fortytwo.smsn.brain.rdf.NoteCollection;
import net.fortytwo.smsn.brain.rdf.NoteReqex;
import net.fortytwo.smsn.brain.rdf.classes.DatedEvent;

import java.util.Collections;
import java.util.regex.Pattern;

public class Log extends NoteCollection {

    public Log() {
        super(
                "log",
                Pattern.compile("log of .+"),
                null,
                new NoteReqex(Collections.singletonList(
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.OneOrMore, DatedEvent.class, Log.class)
                )));
    }
}
