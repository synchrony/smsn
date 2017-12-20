package net.fortytwo.smsn.brain.io.markdown;

import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.query.Model;
import org.commonmark.node.Node;
import org.commonmark.parser.Parser;
import org.commonmark.renderer.html.HtmlRenderer;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class MarkdownParserTest {

    @Test
    public void commonMarkParsesSimpleMarkdown() {
        Parser parser = Parser.builder().build();
        Node document = parser.parse("This is *Sparta*");
        HtmlRenderer renderer = HtmlRenderer.builder().build();
        String doc = renderer.render(document);
        assertEquals("<p>This is <em>Sparta</em></p>\n", doc);
    }

    @Test
    public void completeExampleParsedCorrectly() throws IOException {
        Note root;
        MarkdownParser parser = new MarkdownParser();
        try (InputStream in = MarkdownParserTest.class.getResourceAsStream("markdown-example-1.md")) {
            root = parser.parse(in);
        }

        assertEquals(7, Model.countChildren(root));
    }

    @Test
    public void linksBeforeFirstHeadingAreNotes() throws IOException {
        Note root = parse("This line has [a link](aaaaaaa) and [another link](bbbbbbb).\n" +
                "\n" +
                "# this is a heading\n" +
                "This is [a link under the first heading](ccccccc).");

        assertEquals(3, Model.countChildren(root));
        Note aLink = root.getFirst().get(0);
        assertEquals("a link", aLink.getLabel());
        assertEquals("aaaaaaa", Model.getTopicId(aLink));
        Note anotherLink = root.getFirst().get(1);
        assertEquals("another link", anotherLink.getLabel());
        assertEquals("bbbbbbb", Model.getTopicId(anotherLink));
        Note heading = root.getFirst().get(2);
        assertEquals("this is a heading", heading.getLabel());
        assertEquals(1, Model.countChildren(heading));
        Note linkUnder = heading.getFirst().get(0);
        assertEquals("a link under the first heading", linkUnder.getLabel());
        assertEquals("ccccccc", Model.getTopicId(linkUnder));
    }

    @Test
    public void ordinaryLinesAreNotNotes() throws IOException {
        Note root = parse("This is just a line without links.\n" +
                "\n" +
                "This line has [a link](0000000).");

        assertEquals(1, Model.countChildren(root));
        Note aLink = root.getFirst().get(0);
        assertEquals("a link", aLink.getLabel());
        assertEquals("0000000", Model.getTopicId(aLink));
    }

    @Test
    public void listItemsWithoutLinksAreNotNotes() throws IOException {
        Note root = parse("* list item with no link\n" +
                "* list item [with a link](http://example.org)");

        assertEquals(1, Model.countChildren(root));
        assertEquals("with a link", root.getFirst().get(0).getLabel());
    }

    @Test
    public void headingsWithoutLinksAreNotes() throws IOException {
        Note root = parse("# heading with no link\n" +
                "* [child of heading with no link](bbbbbbb)\n" +
                "# heading [with a link](zzzzzzz)\n" +
                "* [child of heading with a link](aaaaaaa)\n");

        assertEquals(2, Model.countChildren(root));

        Note withoutLink = root.getFirst().get(0);
        assertEquals("heading with no link", withoutLink.getLabel());
        assertNull(Model.getTopicId(withoutLink));
        assertEquals(1, Model.countChildren(withoutLink));

        Note withALink = root.getFirst().get(1);
        assertEquals("with a link", withALink.getLabel());
        assertEquals("zzzzzzz", Model.getTopicId(withALink));
        assertEquals(1, Model.countChildren(withALink));
        Note child = withALink.getFirst().get(0);
        assertEquals("child of heading with a link", child.getLabel());
        assertEquals("aaaaaaa", Model.getTopicId(child));
    }

    @Test
    public void headingWithoutChildrenIsEquivalentToSimpleNote() throws IOException {
        Note root = parse("[simple note](aaaaaaa)\n" +
                "\n" +
                "# heading without content\n" +
                "\n" +
                "# heading with content\n" +
                "\n" +
                "[another note](bbbbbbb)");

        assertEquals(3, Model.countChildren(root));

        Note simpleNote = root.getFirst().get(0);
        assertEquals("simple note", simpleNote.getLabel());
        assertEquals(0, Model.countChildren(simpleNote));
        Note headingWithoutContent = root.getFirst().get(1);
        assertEquals("heading without content", headingWithoutContent.getLabel());
        assertEquals(0, Model.countChildren(headingWithoutContent));
        Note headingWithContent = root.getFirst().get(2);
        assertEquals("heading with content", headingWithContent.getLabel());
        assertEquals(1, Model.countChildren(headingWithContent));
        Note anotherNote = headingWithContent.getFirst().get(0);
        assertEquals("another note", anotherNote.getLabel());
        assertEquals(0, Model.countChildren(anotherNote));
    }

    private Note parse(final String content) throws IOException {
        ByteArrayInputStream input = new ByteArrayInputStream(content.getBytes());

        MarkdownParser parser = new MarkdownParser();
        //parser.setVerbose(true);
        return parser.parse(input);
    }
}
