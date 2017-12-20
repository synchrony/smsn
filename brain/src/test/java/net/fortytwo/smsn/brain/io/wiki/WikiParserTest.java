package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Note;
import org.junit.Test;

import java.io.IOException;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNull;
import static org.junit.Assert.assertNotNull;

public class WikiParserTest extends BrainTestBase {

    private Note exampleNote;

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }

    @Test
    public void attributesAreParsedCorrectly() throws Exception {
        Note root = getExample();

        assertEquals("http://example.org/alias-url-for-this-page", root.getAlias());
        assertEquals("se", root.getShortcut());
        assertEquals(0.75f, root.getWeight());
        assertEquals(0.5f, root.getPriority());
    }

    @Test
    public void transitionalAttributesAreAttachedCorrectly() throws Exception {
        Note root = getExample();

        assertEquals("12345", root.getTopic().getId());
        assertEquals("SmSn syntax example", root.getLabel());
    }

    @Test
    public void textBeginsWithProperty() throws IOException {
        Note root = wikiParser.parse("* Arthur\n" +
                "    @text ```\n" +
                "Here is some text about Arthur.\n" +
                "```\n");

        assertEquals("Arthur", root.getFirst().getFirst().getLabel());
        Note embeddedNote = root.getFirst().getFirst();

        assertEquals("Here is some text about Arthur.", embeddedNote.getText());
    }

    @Test
    public void textIsCopiedVerbatim() throws Exception {
        Note root = getExample();

        assertEquals("Unstructured text may be included between triple backticks, similar to Markdown.\n" +
                        "Any number of lines of text is allowed.",
                root.getText());
    }

    @Test
    public void emptyTextIsIgnored() throws Exception {
        Note root = parseToNote("* token node\n\n");

        assertEquals(1, root.getFirst().length());
        assertNull(root.getText());

        root = parseToNote("* token node\n  \n");
        assertNull(root.getText());
    }

    @Test
    public void nodeHierarchyIsCorrect() throws Exception {
        Note root = getExample();

        assertNotNull(root);
        assertEquals(7, root.getFirst().length());
        assertEquals(5, root.getFirst().get(0).getFirst().length());
    }

    @Test
    public void testRoleIsRespected() throws Exception {
        Note root = getExample();
        assertNull(root.getRole());
        assertNull(root.getFirst().get(0).getRole());

        Note header = root.getFirst().get(5);
        assertEquals(Role.Relation, header.getRole());
        assertNull(header.getFirst().get(0).getRole());

        Note subHeader = header.getFirst().get(3);
        assertEquals(Role.Relation, subHeader.getRole());
        assertEquals("gzScm", subHeader.getTopic().getId());
        assertEquals("additional comments", subHeader.getLabel());
        assertEquals(1, subHeader.getFirst().length());
    }

    @Test
    public void testEmptyLinesAreIgnored() throws Exception {
        Note root = getExample();

        Note whitespaceNode = root.getFirst().get(2);
        assertEquals("white space", whitespaceNode.getLabel());
        assertEquals(4, whitespaceNode.getFirst().length());
        assertEquals("blank lines don't matter", whitespaceNode.getFirst().get(3).getLabel());
    }

    @Test
    public void testWhitespaceIsTrimmed() throws Exception {
        Note root = getExample();

        Note whitespaceNode = root.getFirst().get(2);
        assertEquals("white space", whitespaceNode.getLabel());
        assertEquals("leading and/or trailing whitespace is trimmed",
                whitespaceNode.getFirst().get(2).getLabel());
    }

    @Test
    public void unicodeIsHandledAsExpected() throws IOException {
        Note root = parseToNote(
                "+ :UAk6ejU: gemuetlichkeit\n\u00b7 :hSsMqzT: gem\\u00ftlichkeit\n");
        assertEquals(2, root.getFirst().length());
        assertEquals("gemuetlichkeit", root.getFirst().get(0).getLabel());
        assertEquals("gem\\u00ftlichkeit", root.getFirst().get(1).getLabel());
    }

    @Test
    public void testInvalidIdCharacters() throws Exception {
        Note root = parseToNote("" +
                "* :123@456: the 'ID' of this note contains a character not in [A-Za-z0-9]\n" +
                "* it does not actually become an ID; just more value text");
        assertEquals(2, root.getFirst().length());
        assertNull(root.getFirst().get(0).getTopic());
    }

    @Test
    public void emptyPageIsAllowed() throws Exception {
        Note root = parseToNote("");
        assertEquals(parserTopicId, root.getTopic().getId());
        assertEquals(parserLabel, root.getLabel());
        assertNull(root.getFirst());
        assertEquals(parserSource, root.getSource());
        assertNull(root.getAlias());
        assertNull(root.getText());
        assertNull(root.getPriority());
        assertNull(root.getWeight());
        assertNull(root.getShortcut());
    }

    @Test(expected = IOException.class)
    public void testEmptyValuesNotAllowedForNewNotes() throws Exception {
        parseToNote("* ");
    }

    @Test
    public void testEmptyValuesAllowedForExistingNotes() throws Exception {
        Note root = parseToNote("* :1234567: ");
        assertEquals(1, root.getFirst().length());
        assertEquals("1234567", root.getFirst().get(0).getTopic().getId());
    }

    @Test
    public void testEmptyAliasAttributeAllowed() throws Exception {
        Note root = parseToNote("@alias ");
        assertEquals(WikiFormat.CLEARME, root.getAlias());
    }

    @Test
    public void testEmptyShortcutAttributeAllowed() throws Exception {
        Note root = parseToNote("@shortcut ");
        assertEquals(WikiFormat.CLEARME, root.getShortcut());
    }

    @Test(expected = IOException.class)
    public void testEmptyPriorityAttributeNotAllowed() throws Exception {
        parseToNote("@priority ");
    }

    @Test(expected = IOException.class)
    public void testEmptyWeightAttributeNotAllowed() throws Exception {
        parseToNote("@weight ");
    }

    @Test(expected = IOException.class)
    public void testLineTruncationSequenceNotAllowed() throws Exception {
        parseToNote("" +
                "* this is a note whose value was truncated for readability [...]\n" +
                "   * you wouldn't want to lose the actual value because of a careless copy and paste, would you?");
    }

    @Test
    public void testLegalIds() throws Exception {
        Note root = parseToNote("+ :LTWrf62: courage\n" +
                "+ :COAZgCU: justice\n" +
                "+ :g20vP2u: prudence\n" +
                "+ :Ifkv0cj: temperance\n" +
                "+ :rArdqLh: detachment\n" +
                "+ :pXOAOuS: sincerity\n");
        assertEquals(6, root.getFirst().length());
        assertEquals("LTWrf62", root.getFirst().get(0).getTopic().getId());
        assertEquals("rArdqLh", root.getFirst().get(4).getTopic().getId());

        root = parseToNote("" +
                "* :aaaaa:        IDs as short as 5 bytes are OK, although 16-byte IDs are 'standard'\n" +
                "* :aaaaaaaa: longer IDs are OK, too\n" +
                "* :a: this is not an ID");
        assertEquals(3, root.getFirst().length());
        assertEquals("aaaaa", root.getFirst().get(0).getTopic().getId());
        assertEquals("aaaaaaaa", root.getFirst().get(1).getTopic().getId());
        assertNull(root.getFirst().get(2).getTopic());
    }

    @Test
    public void singleCharacterBulletsAreInterchangeable() throws Exception {
        Note root = getExample();

        Note tree = root.getFirst().get(4);
        assertEquals("ordinary bullets", tree.getLabel());
        assertEquals(5, tree.getFirst().length());
        for (int i = 0; i < tree.getFirst().length(); i++) {
            Note child = tree.getFirst().get(i);
            assertNull(child.getRole());
        }
    }

    @Test
    public void indentationIsRespected() throws Exception {
        Note root = getExample();

        Note indentation = root.getFirst().get(1);
        assertNull(indentation.getTopic());
        assertEquals("indentation", indentation.getLabel());
        assertEquals("and this", indentation.getFirst()
                .get(2).getFirst()
                .get(0).getFirst()
                .get(0).getFirst()
                .get(0).getLabel());
    }

    @Test
    public void idsAreParsedCorrectly() throws Exception {
        Note root = getExample();

        Note ids = root.getFirst().get(3);
        assertEquals("ids", ids.getLabel());
        assertEquals("0txXBm", ids.getFirst().get(0).getTopic().getId());
        assertEquals("cE85nD", ids.getFirst().get(1).getTopic().getId());
    }

    @Test
    public void embeddedPropertiesAreParsedCorrectly() throws IOException {
        Note root = getExample();

        Note noProps = root.getFirst().get(1);
        //assertEquals(DefaultSources.UNIVERSAL, noProps.getSource());
        assertNull(noProps.getSource());

        Note embeddedNote = root.getFirst().get(6);
        assertNotNull(embeddedNote);
        assertEquals("another-source", embeddedNote.getSource());
        assertEquals(0.75f, embeddedNote.getWeight());
        assertEquals(0.5f, embeddedNote.getPriority());
        assertEquals("http://example.org/alias-url-for-this-note", embeddedNote.getAlias());
        assertEquals("ep", embeddedNote.getShortcut());
    }

    private Note getExample() throws IOException {
        if (null == exampleNote) {
            exampleNote = wikiParser.parse(
                    getClass().getResourceAsStream("syntax-example.txt"));
        }

        return exampleNote;
    }
}
