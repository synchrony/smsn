package net.fortytwo.extendo.brain.rdf;

import junit.framework.TestCase;
import net.fortytwo.extendo.brain.rdf.types.AKA;
import net.fortytwo.extendo.brain.rdf.types.BibtexReference;
import net.fortytwo.extendo.brain.rdf.types.Date;
import net.fortytwo.extendo.brain.rdf.types.ISBN;
import net.fortytwo.extendo.brain.rdf.types.OpenCollection;
import net.fortytwo.extendo.brain.rdf.types.Person;
import net.fortytwo.extendo.brain.rdf.types.RFID;
import net.fortytwo.extendo.brain.rdf.types.TODO;
import net.fortytwo.extendo.brain.rdf.types.URL;
import net.fortytwo.extendo.brain.rdf.types.VocabularyTerm;
import net.fortytwo.extendo.brain.rdf.types.WebPage;
import org.junit.Test;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RDFMappingTest extends TestCase {
    @Test
    public void testAKASyntax() throws Exception {
        BottomUpType t = AKA.INSTANCE;
        assertTrue(t.getValueRegex().matcher("aka \"Ix\"").matches());
        assertFalse(t.getValueRegex().matcher("aka Ix").matches());
        assertFalse(t.getValueRegex().matcher("AKA \"Ix\"").matches());
        assertFalse(t.getValueRegex().matcher("AKA\"Ix\"").matches());
    }

    @Test
    public void testBibtexReferenceSyntax() throws Exception {
        BottomUpType t = BibtexReference.INSTANCE;
        assertTrue(t.getValueRegex().matcher("bibtex: @article{}").matches());
        assertTrue(t.getValueRegex().matcher("bibtex: {{{@article{}}}}").matches());
        assertTrue(t.getValueRegex().matcher("bibtex: {{{\n" +
                "@article{}\n" +
                "}}}").matches());
        assertFalse(t.getValueRegex().matcher("bibtex @article{}").matches());
    }

    @Test
    public void testDateSyntax() throws Exception {
        BottomUpType t = Date.INSTANCE;
        assertTrue(t.getValueRegex().matcher("2013-09-17").matches());
        assertFalse(t.getValueRegex().matcher("2013 09 17").matches());
        assertFalse(t.getValueRegex().matcher("2013").matches());
        assertFalse(t.getValueRegex().matcher("9/17/2013").matches());
    }

    @Test
    public void testISBNSyntax() throws Exception {
        BottomUpType t = ISBN.INSTANCE;

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
    }

    @Test
    public void testOpenCollectionSyntax() throws Exception {
        BottomUpType t = OpenCollection.INSTANCE;
        assertTrue(t.getValueRegex().matcher("some things I like about fish").matches());
        assertFalse(t.getValueRegex().matcher("things I like about fish").matches());
        assertFalse(t.getValueRegex().matcher("something I like about fish").matches());
    }

    @Test
    public void testPersonSyntax() throws Exception {
        BottomUpType t = Person.INSTANCE;
        assertTrue(t.getValueRegex().matcher("Arthur Dent").matches());
        assertFalse(t.getValueRegex().matcher("arthur dent").matches());
        assertFalse(t.getValueRegex().matcher("42 Dent").matches());
    }

    @Test
    public void testRFIDSyntax() throws Exception {
        BottomUpType t = RFID.INSTANCE;
        assertTrue(t.getValueRegex().matcher("RFID: E200 1021 3707 0148 2440 1C7C").matches());
        assertFalse(t.getValueRegex().matcher("RFID: 12345").matches());
        assertFalse(t.getValueRegex().matcher("RFID E200 1021 3707 0148 2440 1C7C").matches());
    }

    @Test
    public void testTODOSyntax() throws Exception {
        BottomUpType t = TODO.INSTANCE;
        assertTrue(t.getValueRegex().matcher("TODO: get it done").matches());
        assertFalse(t.getValueRegex().matcher("todo: get it done").matches());
        assertFalse(t.getValueRegex().matcher("TODO tomorrow").matches());
    }

    @Test
    public void testURLSyntax() throws Exception {
        BottomUpType t = URL.INSTANCE;
        assertTrue(t.getValueRegex().matcher("http://example.org/foobar").matches());
        assertTrue(t.getValueRegex().matcher("https://example.org/foobar").matches());
        assertFalse(t.getValueRegex().matcher("git://github.com/joshsh/extendo.git").matches());
    }

    @Test
    public void testVocabularyTermSyntax() throws Exception {
        BottomUpType t = VocabularyTerm.INSTANCE;
        assertTrue(t.getValueRegex().matcher("\"antidisestablishmentarianism\"").matches());
        assertFalse(t.getValueRegex().matcher("antidisestablishmentarianism").matches());
        assertFalse(t.getValueRegex().matcher("\"\"").matches());
    }

    @Test
    public void testWebPageSyntax() throws Exception {
        BottomUpType t = WebPage.INSTANCE;
        assertTrue(t.getValueRegex().matcher("The Least Known Page on the Web (web page)").matches());
        assertFalse(t.getValueRegex().matcher("The Least Known Page on the Web").matches());
    }
}
