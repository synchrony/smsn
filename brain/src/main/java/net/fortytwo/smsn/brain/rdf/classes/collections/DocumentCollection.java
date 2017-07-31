package net.fortytwo.smsn.brain.rdf.classes.collections;

import net.fortytwo.smsn.brain.rdf.NoteCollection;
import net.fortytwo.smsn.brain.rdf.NoteReqex;
import net.fortytwo.smsn.brain.rdf.classes.Document;

import java.util.Arrays;
import java.util.regex.Pattern;

public class DocumentCollection extends NoteCollection {

    public DocumentCollection() {
        super(
                "document-collection",
                Pattern.compile("(some (books|papers) .+)"),
                null,
                new NoteReqex(Arrays.asList(
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.OneOrMore, Document.class, DocumentCollection.class),
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore)
                )));
    }
}
