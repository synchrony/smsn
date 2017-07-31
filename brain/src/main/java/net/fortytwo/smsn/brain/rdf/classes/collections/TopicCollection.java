package net.fortytwo.smsn.brain.rdf.classes.collections;

import net.fortytwo.smsn.brain.rdf.NoteCollection;
import net.fortytwo.smsn.brain.rdf.NoteReqex;
import net.fortytwo.smsn.brain.rdf.classes.DatedEvent;
import net.fortytwo.smsn.brain.rdf.classes.Document;
import net.fortytwo.smsn.brain.rdf.classes.LinkedConcept;
import net.fortytwo.smsn.brain.rdf.classes.Person;
import net.fortytwo.smsn.brain.rdf.classes.Tool;

import java.util.Arrays;
import java.util.regex.Pattern;

public class TopicCollection extends NoteCollection {

    public TopicCollection() {
        super(
                "topic-collection",
                Pattern.compile("some (topics|concepts) from .+"),
                null,
                new NoteReqex(Arrays.asList(
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore, TopicCollection.class),
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore,
                                DatedEvent.class, Document.class, LinkedConcept.class, Person.class, Tool.class),
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore)
                )));
    }
}
