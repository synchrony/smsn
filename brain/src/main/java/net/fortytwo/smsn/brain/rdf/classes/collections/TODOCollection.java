package net.fortytwo.smsn.brain.rdf.classes.collections;

import net.fortytwo.smsn.brain.rdf.NoteCollection;
import net.fortytwo.smsn.brain.rdf.NoteReqex;
import net.fortytwo.smsn.brain.rdf.classes.TODOTask;

import java.util.Arrays;
import java.util.regex.Pattern;

public class TODOCollection extends NoteCollection {

    public TODOCollection() {
        super(
                "todo-collection",
                Pattern.compile("some .+"),
                null,
                new NoteReqex(Arrays.asList(
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.OneOrMore, TODOTask.class, TODOCollection.class),
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore)
                )));
    }
}
