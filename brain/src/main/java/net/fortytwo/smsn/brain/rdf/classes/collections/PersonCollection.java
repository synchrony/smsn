package net.fortytwo.smsn.brain.rdf.classes.collections;

import net.fortytwo.smsn.brain.rdf.NoteCollection;
import net.fortytwo.smsn.brain.rdf.NoteReqex;
import net.fortytwo.smsn.brain.rdf.classes.Person;

import java.util.Arrays;
import java.util.regex.Pattern;

public class PersonCollection extends NoteCollection {

    public PersonCollection() {
        super(
                "person-collection",
                // TODO: this needs a dictionary
                Pattern.compile("(some (.+ )?(people|individuals|students|undergrads|postdocs|professors|staff|visitors)( .+)?)" +
                        "|(.+ children)"),
                null,
                new NoteReqex(Arrays.asList(
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.OneOrMore, Person.class, PersonCollection.class, Person.SocialNetworkCollection.class),
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore)
                )));
    }
}
