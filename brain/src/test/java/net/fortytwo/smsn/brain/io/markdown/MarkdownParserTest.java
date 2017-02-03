package net.fortytwo.smsn.brain.io.markdown;

import net.fortytwo.smsn.brain.model.Note;
import org.commonmark.node.Document;
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
        Parser parser = Parser.builder().build();
        Document document;
        Note root;
        MarkdownParser reader = new MarkdownParser();
        try (InputStream in = MarkdownParserTest.class.getResourceAsStream("markdown-example-1.md")) {
            root = reader.parse(in);
        }

        //mdParser.setVerbose(true);

        assertEquals(7, root.getChildren().size());
    }

    @Test
    public void linksBeforeFirstHeadingAreNotes() throws IOException {
        Note root = parse("This line has [a link](aaaaaaa) and [another link](bbbbbbb).\n" +
                "\n" +
                "# this is a heading\n" +
                "This is [a link under the first heading](ccccccc).");

        assertEquals(3, root.getChildren().size());
        Note aLink = root.getChildren().get(0);
        assertEquals("a link", aLink.getTitle());
        assertEquals("aaaaaaa", aLink.getId());
        Note anotherLink = root.getChildren().get(1);
        assertEquals("another link", anotherLink.getTitle());
        assertEquals("bbbbbbb", anotherLink.getId());
        Note heading = root.getChildren().get(2);
        assertEquals("this is a heading", heading.getTitle());
        assertEquals(1, heading.getChildren().size());
        Note linkUnder = heading.getChildren().get(0);
        assertEquals("a link under the first heading", linkUnder.getTitle());
        assertEquals("ccccccc", linkUnder.getId());
    }

    @Test
    public void ordinaryLinesAreNotNotes() throws IOException {
        Note root = parse("This is just a line without links.\n" +
                "\n" +
                "This line has [a link](0000000).");

        assertEquals(1, root.getChildren().size());
        Note aLink = root.getChildren().get(0);
        assertEquals("a link", aLink.getTitle());
        assertEquals("0000000", aLink.getId());
    }

    @Test
    public void listItemsWithoutLinksAreNotNotes() throws IOException {
        Note root = parse("* list item with no link\n" +
                "* list item [with a link](http://example.org)");

        assertEquals(1, root.getChildren().size());
        assertEquals("with a link", root.getChildren().get(0).getTitle());
    }

    @Test
    public void headingsWithoutLinksAreNotes() throws IOException {
        Note root = parse("# heading with no link\n" +
                "* [child of heading with no link](bbbbbbb)\n" +
                "# heading [with a link](zzzzzzz)\n" +
                "* [child of heading with a link](aaaaaaa)\n");

        assertEquals(2, root.getChildren().size());

        Note withoutLink = root.getChildren().get(0);
        assertEquals("heading with no link", withoutLink.getTitle());
        assertNull(withoutLink.getId());
        assertEquals(1, withoutLink.getChildren().size());

        Note withALink = root.getChildren().get(1);
        assertEquals("with a link", withALink.getTitle());
        assertEquals("zzzzzzz", withALink.getId());
        assertEquals(1, withALink.getChildren().size());
        Note child = withALink.getChildren().get(0);
        assertEquals("child of heading with a link", child.getTitle());
        assertEquals("aaaaaaa", child.getId());
    }

    @Test
    public void headingWithoutChildrenIsEquivalentToSimpleAtom() throws IOException {
        Note root = parse("[simple atom](aaaaaaa)\n" +
                "\n" +
                "# heading without content\n" +
                "\n" +
                "# heading with content\n" +
                "\n" +
                "[another atom](bbbbbbb)");

        assertEquals(3, root.getChildren().size());

        Note simpleAtom = root.getChildren().get(0);
        assertEquals("simple atom", simpleAtom.getTitle());
        assertEquals(0, simpleAtom.getChildren().size());
        Note headingWithoutContent = root.getChildren().get(1);
        assertEquals("heading without content", headingWithoutContent.getTitle());
        assertEquals(0, headingWithoutContent.getChildren().size());
        Note headingWithContent = root.getChildren().get(2);
        assertEquals("heading with content", headingWithContent.getTitle());
        assertEquals(1, headingWithContent.getChildren().size());
        Note anotherAtom = headingWithContent.getChildren().get(0);
        assertEquals("another atom", anotherAtom.getTitle());
        assertEquals(0, anotherAtom.getChildren().size());
    }

    private Note parse(final String content) throws IOException {
        ByteArrayInputStream input = new ByteArrayInputStream(content.getBytes());

        MarkdownParser parser = new MarkdownParser();
        //parser.setVerbose(true);
        return parser.parse(input);
    }
}
