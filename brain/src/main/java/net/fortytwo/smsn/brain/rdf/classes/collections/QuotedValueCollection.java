package net.fortytwo.smsn.brain.rdf.classes.collections;

import net.fortytwo.smsn.brain.rdf.NoteCollection;
import net.fortytwo.smsn.brain.rdf.NoteReqex;
import net.fortytwo.smsn.brain.rdf.classes.QuotedValue;

import java.util.Arrays;
import java.util.regex.Pattern;

public class QuotedValueCollection extends NoteCollection {

    public QuotedValueCollection() {
        super(
                "quoted-value-collection",
                Pattern.compile("some .+"),
                null,
                new NoteReqex(Arrays.asList(
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.OneOrMore, QuotedValue.class, QuotedValueCollection.class),
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore)
                )));
    }
}
