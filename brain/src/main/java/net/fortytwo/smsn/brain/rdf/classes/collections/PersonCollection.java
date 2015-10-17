package net.fortytwo.smsn.brain.rdf.classes.collections;

import net.fortytwo.smsn.brain.rdf.AtomCollection;
import net.fortytwo.smsn.brain.rdf.AtomRegex;
import net.fortytwo.smsn.brain.rdf.classes.Person;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PersonCollection extends AtomCollection {

    public PersonCollection() {
        super(
                "person-collection",
                // TODO: this needs a dictionary
                Pattern.compile("(some (.+ )?(people|individuals|students|undergrads|postdocs|professors|staff|visitors)( .+)?)" +
                        "|(.+ children)"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.OneOrMore, Person.class, PersonCollection.class, Person.SocialNetworkCollection.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore)
                )));
    }
}
