package net.fortytwo.extendo.brain.rdf.classes.collections;

import net.fortytwo.extendo.brain.rdf.AtomCollection;
import net.fortytwo.extendo.brain.rdf.AtomRegex;
import net.fortytwo.extendo.brain.rdf.classes.Document;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class DocumentAboutTopicCollection extends AtomCollection {

    // TODO: unit tests for documents about plain and sub-typed topics
    public DocumentAboutTopicCollection() {
        super(
                "documents-about-collection",
                Pattern.compile("some (papers|books|articles|documents) (about|on) .+"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore, DocumentCollection.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore,
                                Document.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore)
                )));
    }
}
