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

public class MarkdownReaderTest {

    @Test
    public void commonMarkParsesSimpleMarkdown() {
        Parser parser = Parser.builder().build();
        Node document = parser.parse("This is *Sparta*");
        HtmlRenderer renderer = HtmlRenderer.builder().build();
        String doc = renderer.render(document);  // "<p>This is <em>Sparta</em></p>\n"
        System.out.println("doc: " + doc);
    }

    @Test
    public void completeExampleParsedCorrectly() throws IOException {
        Parser parser = Parser.builder().build();
        Document document;
        Note root;
        MarkdownReader reader = new MarkdownReader();
        try (InputStream in = MarkdownReaderTest.class.getResourceAsStream("markdown-example-1.md")) {
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
        assertEquals("a link", aLink.getValue());
        assertEquals("aaaaaaa", aLink.getId());
        Note anotherLink = root.getChildren().get(1);
        assertEquals("another link", anotherLink.getValue());
        assertEquals("bbbbbbb", anotherLink.getId());
        Note heading = root.getChildren().get(2);
        assertEquals("this is a heading", heading.getValue());
        assertEquals(1, heading.getChildren().size());
        Note linkUnder = heading.getChildren().get(0);
        assertEquals("a link under the first heading", linkUnder.getValue());
        assertEquals("ccccccc", linkUnder.getId());
    }

    @Test
    public void ordinaryLinesAreNotNotes() throws IOException {
        Note root = parse("This is just a line without links.\n" +
                "\n" +
                "This line has [a link](0000000).");

        assertEquals(1, root.getChildren().size());
        Note aLink = root.getChildren().get(0);
        assertEquals("a link", aLink.getValue());
        assertEquals("0000000", aLink.getId());
    }

    @Test
    public void listItemsWithoutLinksAreNotNotes() throws IOException {
        Note root = parse("* list item with no link\n" +
                "* list item [with a link](http://example.org)");

        assertEquals(1, root.getChildren().size());
        assertEquals("with a link", root.getChildren().get(0).getValue());
    }

    @Test
    public void headingsWithoutLinksAreNotes() throws IOException {
        Note root = parse("# heading with no link\n" +
                "* [child of heading with no link](bbbbbbb)\n" +
                "# heading [with a link](zzzzzzz)\n" +
                "* [child of heading with a link](aaaaaaa)\n");

        assertEquals(2, root.getChildren().size());

        Note withoutLink = root.getChildren().get(0);
        assertEquals("heading with no link", withoutLink.getValue());
        assertNull(withoutLink.getId());
        assertEquals(1, withoutLink.getChildren().size());

        Note withALink = root.getChildren().get(1);
        assertEquals("with a link", withALink.getValue());
        assertEquals("zzzzzzz", withALink.getId());
        assertEquals(1, withALink.getChildren().size());
        Note child = withALink.getChildren().get(0);
        assertEquals("child of heading with a link", child.getValue());
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
        assertEquals("simple atom", simpleAtom.getValue());
        assertEquals(0, simpleAtom.getChildren().size());
        Note headingWithoutContent = root.getChildren().get(1);
        assertEquals("heading without content", headingWithoutContent.getValue());
        assertEquals(0, headingWithoutContent.getChildren().size());
        Note headingWithContent = root.getChildren().get(2);
        assertEquals("heading with content", headingWithContent.getValue());
        assertEquals(1, headingWithContent.getChildren().size());
        Note anotherAtom = headingWithContent.getChildren().get(0);
        assertEquals("another atom", anotherAtom.getValue());
        assertEquals(0, anotherAtom.getChildren().size());
    }

    private Note parse(final String content) throws IOException {
        ByteArrayInputStream input = new ByteArrayInputStream(content.getBytes());

        MarkdownReader parser = new MarkdownReader();
        //parser.setVerbose(true);
        return parser.parse(input);
    }
}
