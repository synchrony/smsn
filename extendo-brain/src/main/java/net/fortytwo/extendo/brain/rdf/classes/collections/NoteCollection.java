package net.fortytwo.extendo.brain.rdf.classes.collections;

import net.fortytwo.extendo.brain.rdf.AtomCollection;
import net.fortytwo.extendo.brain.rdf.AtomRegex;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteCollection extends AtomCollection {
    public static final NoteCollection INSTANCE = new NoteCollection();

    public NoteCollection() {
        super(
                "notes-from",
                Pattern.compile("some notes from .+"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore, GenericCollection.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore)
                )));
    }
}
