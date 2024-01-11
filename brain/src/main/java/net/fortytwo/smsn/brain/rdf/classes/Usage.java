package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.rdf.NoteReqex;
import net.fortytwo.smsn.brain.rdf.SimpleNoteClass;

import java.util.Collections;
import java.util.regex.Pattern;

/**
 * A collection of how-to notes relating to a tool
 */
public class Usage extends SimpleNoteClass {

    public Usage() {
        super(
                "usage",
                Pattern.compile(".+ usage"),
                null,
                new NoteReqex(Collections.singletonList(
                        new NoteReqex.El(null,
                                // usage implies some content, regardless of what it is
                                NoteReqex.Modifier.OneOrMore))));
    }
}
