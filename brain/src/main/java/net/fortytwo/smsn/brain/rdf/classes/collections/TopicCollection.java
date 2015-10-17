package net.fortytwo.smsn.brain.rdf.classes.collections;

import net.fortytwo.smsn.brain.rdf.AtomCollection;
import net.fortytwo.smsn.brain.rdf.AtomRegex;
import net.fortytwo.smsn.brain.rdf.classes.DatedEvent;
import net.fortytwo.smsn.brain.rdf.classes.Document;
import net.fortytwo.smsn.brain.rdf.classes.LinkedConcept;
import net.fortytwo.smsn.brain.rdf.classes.Person;
import net.fortytwo.smsn.brain.rdf.classes.Tool;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TopicCollection extends AtomCollection {

    public TopicCollection() {
        super(
                "topic-collection",
                Pattern.compile("some (topics|concepts) from .+"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore, TopicCollection.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore,
                                DatedEvent.class, Document.class, LinkedConcept.class, Person.class, Tool.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore)
                )));
    }
}
