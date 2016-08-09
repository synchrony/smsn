package net.fortytwo.smsn.brain.rdf.classes.collections;

import net.fortytwo.smsn.brain.rdf.AtomCollection;
import net.fortytwo.smsn.brain.rdf.AtomRegex;
import net.fortytwo.smsn.brain.rdf.classes.TODOTask;

import java.util.Arrays;
import java.util.regex.Pattern;

public class TODOCollection extends AtomCollection {

    public TODOCollection() {
        super(
                "todo-collection",
                Pattern.compile("some .+"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.OneOrMore, TODOTask.class, TODOCollection.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore)
                )));
    }
}
