package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import org.junit.Test;

import java.io.IOException;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNull;
import static org.junit.Assert.assertNotNull;

public class WikiParserTest extends BrainTestBase {

    private Page examplePage;

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }

    @Test
    public void pageAttributesAreParsedCorrectly() throws Exception {
        Page page = getExample();

        assertEquals("http://example.org/alias-url-for-this-page", page.getAlias());
        assertEquals("se", page.getShortcut());
        assertEquals(0.75f, page.getWeight());
        assertEquals(0.5f, page.getPriority());
    }

    @Test
    public void transitionalAttributesAreAttachedCorrectly() throws Exception {
        Page page = getExample();

        assertEquals("12345", page.getContent().getValue().getTarget().getId());
        assertEquals("SmSn syntax example", page.getContent().getValue().getLabel());
    }

    @Test
    public void textBeginsWithProperty() throws IOException {
        Page page = wikiParser.parse("* Arthur\n" +
                "@text\n" +
                "Here is some text about Arthur.");

        assertEquals("Arthur", page.getContent().getChildren().getFirst().getValue().getLabel());
        assertEquals("Here is some text about Arthur.", page.getText());
    }

    @Test
    public void textIsCopiedVerbatim() throws Exception {
        Page page = getExample();

        assertEquals("Unstructured text may be included after the first blank line.\n" +
                        "Any number of lines of text are allowed.",
                page.getText());
    }

    @Test
    public void emptyTextIsIgnored() throws Exception {
        Page page = parseToPage("* token node\n\n");

        assertEquals(1, page.getContent().getChildren().length());
        assertNull(page.getText());

        page = parseToPage("* token node\n  \n");
        assertNull(page.getText());
    }

    @Test
    public void nodeHierarchyIsCorrect() throws Exception {
        Page page = getExample();

        assertNotNull(page.getContent());
        assertNotNull(page.getContent().getValue());
        assertEquals(7, page.getContent().getChildren().length());
        assertEquals(5, page.getContent().getChildren().get(0).getChildren().length());
    }

    @Test
    public void testRoleIsRespected() throws Exception {
        Page page = getExample();
        assertNull(page.getContent().getValue().getRole());
        assertNull(page.getContent().getChildren().get(0).getValue().getRole());

        TreeNode<Link> header = page.getContent().getChildren().get(5);
        assertEquals(Role.Relation, header.getValue().getRole());
        assertNull(header.getChildren().get(0).getValue().getRole());

        TreeNode<Link> subHeader = header.getChildren().get(3);
        assertEquals(Role.Relation, subHeader.getValue().getRole());
        assertEquals("gzScm", subHeader.getValue().getTarget().getId());
        assertEquals("additional comments", subHeader.getValue().getLabel());
        assertEquals(1, subHeader.getChildren().length());
    }

    @Test
    public void testEmptyLinesAreIgnored() throws Exception {
        Page page = getExample();

        TreeNode<Link> whitespaceNode = page.getContent().getChildren().get(2);
        assertEquals("white space", whitespaceNode.getValue().getLabel());
        assertEquals(4, whitespaceNode.getChildren().length());
        assertEquals("blank lines don't matter", whitespaceNode.getChildren().get(3).getValue().getLabel());
    }

    @Test
    public void testWhitespaceIsTrimmed() throws Exception {
        Page page = getExample();

        TreeNode<Link> whitespaceNode = page.getContent().getChildren().get(2);
        assertEquals("white space", whitespaceNode.getValue().getLabel());
        assertEquals("leading and/or trailing whitespace is trimmed",
                whitespaceNode.getChildren().get(2).getValue().getLabel());
    }

    @Test
    public void unicodeIsHandledAsExpected() throws IOException {
        Page page = parseToPage(
                "+ :UAk6ejU: gemuetlichkeit\n\u00b7 :hSsMqzT: gem\\u00ftlichkeit\n");
        assertEquals(2, page.getContent().getChildren().length());
        assertEquals("gemuetlichkeit", page.getContent().getChildren().get(0).getValue().getLabel());
        assertEquals("gem\\u00ftlichkeit", page.getContent().getChildren().get(1).getValue().getLabel());
    }

    @Test
    public void testInvalidIdCharacters() throws Exception {
        Page page = parseToPage("" +
                "* :123@456: the 'ID' of this note contains a character not in [A-Za-z0-9]\n" +
                "* it does not actually become an ID; just more value text");
        assertEquals(2, page.getContent().getChildren().length());
        assertNull(page.getContent().getChildren().get(0).getValue().getTarget());
    }

    @Test
    public void emptyPageIsAllowed() throws Exception {
        Page page = parseToPage("");
        assertEquals(parserTopicId, page.getContent().getValue().getTarget().getId());
        assertEquals(parserLabel, page.getContent().getValue().getLabel());
        assertNull(page.getContent().getChildren());
        assertEquals(parserSource, page.getSource());
        assertNull(page.getAlias());
        assertNull(page.getText());
        assertNull(page.getPriority());
        assertNull(page.getWeight());
        assertNull(page.getShortcut());
    }

    @Test(expected = IOException.class)
    public void testEmptyValuesNotAllowedForNewNotes() throws Exception {
        parseToTree("* ");
    }

    @Test
    public void testEmptyValuesAllowedForExistingNotes() throws Exception {
        Page page = parseToPage("* :1234567: ");
        assertEquals(1, page.getContent().getChildren().length());
        assertEquals("1234567", page.getContent().getChildren().get(0).getValue().getTarget().getId());
    }

    @Test
    public void testEmptyAliasAttributeAllowed() throws Exception {
        Page page = parseToPage("@alias ");
        assertEquals(WikiFormat.CLEARME, page.getAlias());
    }

    @Test
    public void testEmptyShortcutAttributeAllowed() throws Exception {
        Page page = parseToPage("@shortcut ");
        assertEquals(WikiFormat.CLEARME, page.getShortcut());
    }

    @Test(expected = IOException.class)
    public void testEmptyPriorityAttributeNotAllowed() throws Exception {
        parseToTree("@priority ");
    }

    @Test(expected = IOException.class)
    public void testEmptyWeightAttributeNotAllowed() throws Exception {
        parseToTree("@weight ");
    }

    @Test(expected = IOException.class)
    public void testLineTruncationSequenceNotAllowed() throws Exception {
        parseToTree("" +
                "* this is a note whose value was truncated for readability [...]\n" +
                "   * you wouldn't want to lose the actual value because of a careless copy and paste, would you?");
    }

    @Test
    public void testLegalIds() throws Exception {
        Page page = parseToPage("+ :LTWrf62: courage\n" +
                "+ :COAZgCU: justice\n" +
                "+ :g20vP2u: prudence\n" +
                "+ :Ifkv0cj: temperance\n" +
                "+ :rArdqLh: detachment\n" +
                "+ :pXOAOuS: sincerity\n");
        assertEquals(6, page.getContent().getChildren().length());
        assertEquals("LTWrf62", page.getContent().getChildren().get(0).getValue().getTarget().getId());
        assertEquals("rArdqLh", page.getContent().getChildren().get(4).getValue().getTarget().getId());

        page = parseToPage("" +
                "* :aaaaa:        IDs as short as 5 bytes are OK, although 16-byte IDs are 'standard'\n" +
                "* :aaaaaaaa: longer IDs are OK, too\n" +
                "* :a: this is not an ID");
        assertEquals(3, page.getContent().getChildren().length());
        assertEquals("aaaaa", page.getContent().getChildren().get(0).getValue().getTarget().getId());
        assertEquals("aaaaaaaa", page.getContent().getChildren().get(1).getValue().getTarget().getId());
        assertNull(page.getContent().getChildren().get(2).getValue().getTarget());
    }

    @Test
    public void singleCharacterBulletsAreInterchangeable() throws Exception {
        Page page = getExample();

        TreeNode<Link> tree = page.getContent().getChildren().get(4);
        assertEquals("ordinary bullets", tree.getValue().getLabel());
        assertEquals(5, tree.getChildren().length());
        for (int i = 0; i < tree.getChildren().length(); i++) {
            TreeNode<Link> child = tree.getChildren().get(i);
            assertNull(child.getValue().getRole());
        }
    }

    @Test
    public void indentationIsRespected() throws Exception {
        Page page = getExample();

        TreeNode<Link> indentation = page.getContent().getChildren().get(1);
        assertNull(indentation.getValue().getTarget());
        assertEquals("indentation", indentation.getValue().getLabel());
        assertEquals("and this", indentation.getChildren()
                .get(2).getChildren()
                .get(0).getChildren()
                .get(0).getChildren()
                .get(0).getValue().getLabel());
    }

    @Test
    public void idsAreParsedCorrectly() throws Exception {
        Page page = getExample();

        TreeNode<Link> ids = page.getContent().getChildren().get(3);
        assertEquals("ids", ids.getValue().getLabel());
        assertEquals("0txXBm", ids.getChildren().get(0).getValue().getTarget().getId());
        assertEquals("cE85nD", ids.getChildren().get(1).getValue().getTarget().getId());
    }

    @Test
    public void embeddedPropertiesAreParsedCorrectly() throws IOException {
        Page page = getExample();

        TreeNode<Link> noProps = page.getContent().getChildren().get(1);
        //assertEquals(DefaultSources.UNIVERSAL, noProps.getValue().getPage().getSource());
        assertNull(noProps.getValue().getPage().getSource());

        TreeNode<Link> props = page.getContent().getChildren().get(6);
        Page embeddedPage = props.getValue().getPage();
        assertNotNull(embeddedPage);
        assertEquals("another-source", embeddedPage.getSource());
        assertEquals(0.75f, embeddedPage.getWeight());
        assertEquals(0.5f, embeddedPage.getPriority());
        assertEquals("http://example.org/alias-url-for-this-note", embeddedPage.getAlias());
        assertEquals("ep", embeddedPage.getShortcut());
    }

    private Page getExample() throws IOException {
        if (null == examplePage) {
            examplePage = wikiParser.parse(
                    getClass().getResourceAsStream("syntax-example.txt"));
        }

        return examplePage;
    }
}
