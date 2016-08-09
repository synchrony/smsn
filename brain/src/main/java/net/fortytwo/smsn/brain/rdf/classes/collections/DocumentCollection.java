package net.fortytwo.smsn.brain.rdf.classes.collections;

import net.fortytwo.smsn.brain.rdf.AtomCollection;
import net.fortytwo.smsn.brain.rdf.AtomRegex;
import net.fortytwo.smsn.brain.rdf.classes.Document;

import java.util.Arrays;
import java.util.regex.Pattern;

public class DocumentCollection extends AtomCollection {

    public DocumentCollection() {
        super(
                "document-collection",
                Pattern.compile("(some (books|papers) .+)"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.OneOrMore, Document.class, DocumentCollection.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore)
                )));
    }
}
