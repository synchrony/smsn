package net.fortytwo.smsn.brain.rdf.classes.collections;

import net.fortytwo.smsn.brain.rdf.NoteCollection;
import net.fortytwo.smsn.brain.rdf.NoteReqex;
import net.fortytwo.smsn.brain.rdf.classes.Document;

import java.util.Arrays;
import java.util.regex.Pattern;

public class DocumentAboutTopicCollection extends NoteCollection {

    // TODO: unit tests for documents about plain and sub-typed topics
    public DocumentAboutTopicCollection() {
        super(
                "documents-about-collection",
                Pattern.compile("some (papers|books|articles|documents) (about|on) .+"),
                null,
                new NoteReqex(Arrays.asList(
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore, DocumentCollection.class),
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore,
                                Document.class),
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore)
                )));
    }
}
