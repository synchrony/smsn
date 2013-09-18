package net.fortytwo.extendo.brain.rdf;

import junit.framework.TestCase;
import net.fortytwo.extendo.brain.rdf.types.AKA;
import net.fortytwo.extendo.brain.rdf.types.Date;
import net.fortytwo.extendo.brain.rdf.types.OpenCollection;
import net.fortytwo.extendo.brain.rdf.types.Person;
import net.fortytwo.extendo.brain.rdf.types.WebPage;
import org.junit.Test;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RDFMappingTest extends TestCase {
    @Test
    public void testAKASyntax() throws Exception {
        BottomUpType t = new AKA();
        assertTrue(t.getValueRegex().matcher("aka \"Ix\"").matches());
        assertFalse(t.getValueRegex().matcher("aka Ix").matches());
        assertFalse(t.getValueRegex().matcher("AKA \"Ix\"").matches());
        assertFalse(t.getValueRegex().matcher("AKA\"Ix\"").matches());
    }

    @Test
    public void testDateSyntax() throws Exception {
        BottomUpType t = new Date();
        assertTrue(t.getValueRegex().matcher("2013-09-17").matches());
        assertFalse(t.getValueRegex().matcher("2013 09 17").matches());
        assertFalse(t.getValueRegex().matcher("2013").matches());
        assertFalse(t.getValueRegex().matcher("9/17/2013").matches());
    }

    @Test
    public void testOpenCollectionSyntax() throws Exception {
        BottomUpType t = new OpenCollection();
        assertTrue(t.getValueRegex().matcher("some things I like about fish").matches());
        assertFalse(t.getValueRegex().matcher("things I like about fish").matches());
        assertFalse(t.getValueRegex().matcher("something I like about fish").matches());
    }

    @Test
    public void testPersonSyntax() throws Exception {
        BottomUpType t = new Person();
        assertTrue(t.getValueRegex().matcher("Arthur Dent").matches());
        assertFalse(t.getValueRegex().matcher("arthur dent").matches());
        assertFalse(t.getValueRegex().matcher("42 Dent").matches());
    }

    @Test
    public void testWebPageSyntax() throws Exception {
        BottomUpType t = new WebPage();
        assertTrue(t.getValueRegex().matcher("The Least Known Page on the Web (web page)").matches());
        assertFalse(t.getValueRegex().matcher("The Least Known Page on the Web").matches());
    }
}
