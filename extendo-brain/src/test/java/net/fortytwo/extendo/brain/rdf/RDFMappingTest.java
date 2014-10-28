package net.fortytwo.extendo.brain.rdf;

import junit.framework.TestCase;

import net.fortytwo.extendo.brain.rdf.classes.AKAReference;
import net.fortytwo.extendo.brain.rdf.classes.BibtexReference;
import net.fortytwo.extendo.brain.rdf.classes.Date;
import net.fortytwo.extendo.brain.rdf.classes.ISBNReference;
import net.fortytwo.extendo.brain.rdf.classes.Person;
import net.fortytwo.extendo.brain.rdf.classes.QuotedValue;
import net.fortytwo.extendo.brain.rdf.classes.RFIDReference;
import net.fortytwo.extendo.brain.rdf.classes.TODOTask;
import net.fortytwo.extendo.brain.rdf.classes.URLReference;
import net.fortytwo.extendo.brain.rdf.classes.WebPage;
import net.fortytwo.extendo.brain.rdf.classes.collections.GenericCollection;
import org.junit.Test;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RDFMappingTest extends TestCase {
    @Test
    public void testAKASyntax() throws Exception {
        AtomClass t = AKAReference.INSTANCE;

        /* TODO
        assertTrue(t.getValueRegex().matcher("aka \"Ix\"").matches());
        assertFalse(t.getValueRegex().matcher("aka \"\"Ix\"").matches());
        assertFalse(t.getValueRegex().matcher("aka Ix").matches());
        assertFalse(t.getValueRegex().matcher("AKA \"Ix\"").matches());
        assertFalse(t.getValueRegex().matcher("AKA\"Ix\"").matches());

        assertTrue(t.getValueRegex().matcher("aka \"Foo\", \"Bar\"").matches());
        assertFalse(t.getValueRegex().matcher("aka \"Foo\",\"Bar\"").matches());
        */
    }

    @Test
    public void testBibtexReferenceSyntax() throws Exception {
        AtomClass t = BibtexReference.INSTANCE;

        /* TODO
        assertTrue(t.getValueRegex().matcher("bibtex: @article{}").matches());
        assertTrue(t.getValueRegex().matcher("bibtex: {{{@article{}}}}").matches());
        assertTrue(t.getValueRegex().matcher("bibtex: {{{\n" +
                "@article{}\n" +
                "}}}").matches());
        assertFalse(t.getValueRegex().matcher("bibtex @article{}").matches());
        */
    }

    @Test
    public void testDateSyntax() throws Exception {
        AtomClass t = Date.INSTANCE;

        /* TODO
        assertTrue(t.getValueRegex().matcher("2013-09-17").matches());
        assertFalse(t.getValueRegex().matcher("2013 09 17").matches());
        assertFalse(t.getValueRegex().matcher("2013").matches());
        assertFalse(t.getValueRegex().matcher("9/17/2013").matches());
        */
    }

    @Test
    public void testISBNSyntax() throws Exception {
        AtomClass t = ISBNReference.INSTANCE;

        /* TODO
        // valid ISBN-10
        assertTrue(t.getValueRegex().matcher("ISBN: 0-674-01846-X").matches());
        assertTrue(t.additionalConstraintsSatisfied("ISBN: 0-674-01846-X"));

        // valid ISBN-13
        assertTrue(t.getValueRegex().matcher("ISBN: 978-7-5619-1129-7").matches());
        assertTrue(t.additionalConstraintsSatisfied("ISBN: 978-7-5619-1129-7"));

        // spaces instead of dashes are OK
        assertTrue(t.getValueRegex().matcher("ISBN: 978 0 335 21344 3").matches());
        assertTrue(t.additionalConstraintsSatisfied("ISBN: 978 0 335 21344 3"));

        // omission of spaces and dashes is OK
        assertTrue(t.getValueRegex().matcher("ISBN: 089744969X").matches());
        assertTrue(t.additionalConstraintsSatisfied("ISBN: 089744969X"));

        // invalid ISBN-10 (bad checksum)
        assertTrue(t.getValueRegex().matcher("ISBN: 0-674-01846-0").matches());
        assertFalse(t.additionalConstraintsSatisfied("ISBN: 0-674-01846-0"));

        // invalid ISBN-13 (bad checksum)
        assertTrue(t.getValueRegex().matcher("ISBN: 978-7-5619-1129-9").matches());
        assertFalse(t.additionalConstraintsSatisfied("ISBN: 978-7-5619-1129-9"));

        // invalid top-level syntax
        assertFalse(t.getValueRegex().matcher("isbn: 0-674-01846-X").matches());
        assertFalse(t.getValueRegex().matcher("0-674-01846-X").matches());
        */
    }

    @Test
    public void testOpenCollectionSyntax() throws Exception {
        AtomClass t = GenericCollection.INSTANCE;

        /* TODO
        assertTrue(t.getValueRegex().matcher("some things I like about fish").matches());
        assertFalse(t.getValueRegex().matcher("things I like about fish").matches());
        assertFalse(t.getValueRegex().matcher("something I like about fish").matches());
        */
    }

    @Test
    public void testPersonSyntax() throws Exception {
        AtomClass t = Person.INSTANCE;

        /*
        assertTrue(t.getValueRegex().matcher("Arthur Dent").matches());
        assertFalse(t.getValueRegex().matcher("arthur dent").matches());
        assertFalse(t.getValueRegex().matcher("42 Dent").matches());
        */
    }

    @Test
    public void testRFIDSyntax() throws Exception {
        AtomClass t = RFIDReference.INSTANCE;

        /*
        assertTrue(t.getValueRegex().matcher("RFID: E200 1021 3707 0148 2440 1C7C").matches());
        assertFalse(t.getValueRegex().matcher("RFID: 12345").matches());
        assertFalse(t.getValueRegex().matcher("RFID E200 1021 3707 0148 2440 1C7C").matches());
        */
    }

    @Test
    public void testTODOSyntax() throws Exception {
        AtomClass t = TODOTask.INSTANCE;

        /*
        assertTrue(t.getValueRegex().matcher("TODO: get it done").matches());
        assertFalse(t.getValueRegex().matcher("todo: get it done").matches());
        assertFalse(t.getValueRegex().matcher("TODO tomorrow").matches());
        */
    }

    @Test
    public void testURLSyntax() throws Exception {
        AtomClass t = URLReference.INSTANCE;

        /*
        assertTrue(t.getValueRegex().matcher("http://example.org/foobar").matches());
        assertTrue(t.getValueRegex().matcher("https://example.org/foobar").matches());
        assertFalse(t.getValueRegex().matcher("git://github.com/joshsh/extendo.git").matches());
        */
    }

    @Test
    public void testVocabularyTermSyntax() throws Exception {
        AtomClass t = QuotedValue.INSTANCE;

        /*
        assertTrue(t.getValueRegex().matcher("\"antidisestablishmentarianism\"").matches());
        assertFalse(t.getValueRegex().matcher("antidisestablishmentarianism").matches());
        assertFalse(t.getValueRegex().matcher("\"\"").matches());
        */
    }

    @Test
    public void testWebPageSyntax() throws Exception {
        AtomClass t = WebPage.INSTANCE;

        /*
        assertTrue(t.getValueRegex().matcher("The Least Known Page on the Web (web page)").matches());
        assertFalse(t.getValueRegex().matcher("The Least Known Page on the Web").matches());
        */
    }
}
