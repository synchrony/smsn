package net.fortytwo.smsn.brain.io.markdown;

import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
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
        TreeNode<Link> root;
        MarkdownParser parser = new MarkdownParser();
        try (InputStream in = MarkdownParserTest.class.getResourceAsStream("markdown-example-1.md")) {
            root = parser.parse(in).getContent();
        }

        assertEquals(7, TreeViews.countChildren(root));
    }

    @Test
    public void linksBeforeFirstHeadingAreNotes() throws IOException {
        TreeNode<Link> root = parse("This line has [a link](aaaaaaa) and [another link](bbbbbbb).\n" +
                "\n" +
                "# this is a heading\n" +
                "This is [a link under the first heading](ccccccc).");

        assertEquals(3, TreeViews.countChildren(root));
        TreeNode<Link> aLink = root.getChildren().get(0);
        assertEquals("a link", TreeViews.getTitle(aLink));
        assertEquals("aaaaaaa", TreeViews.getId(aLink));
        TreeNode<Link> anotherLink = root.getChildren().get(1);
        assertEquals("another link", TreeViews.getTitle(anotherLink));
        assertEquals("bbbbbbb", TreeViews.getId(anotherLink));
        TreeNode<Link> heading = root.getChildren().get(2);
        assertEquals("this is a heading", TreeViews.getTitle(heading));
        assertEquals(1, TreeViews.countChildren(heading));
        TreeNode<Link> linkUnder = heading.getChildren().get(0);
        assertEquals("a link under the first heading", TreeViews.getTitle(linkUnder));
        assertEquals("ccccccc", TreeViews.getId(linkUnder));
    }

    @Test
    public void ordinaryLinesAreNotNotes() throws IOException {
        TreeNode<Link> root = parse("This is just a line without links.\n" +
                "\n" +
                "This line has [a link](0000000).");

        assertEquals(1, TreeViews.countChildren(root));
        TreeNode<Link> aLink = root.getChildren().get(0);
        assertEquals("a link", TreeViews.getTitle(aLink));
        assertEquals("0000000", TreeViews.getId(aLink));
    }

    @Test
    public void listItemsWithoutLinksAreNotNotes() throws IOException {
        TreeNode<Link> root = parse("* list item with no link\n" +
                "* list item [with a link](http://example.org)");

        assertEquals(1, TreeViews.countChildren(root));
        assertEquals("with a link", TreeViews.getTitle(root.getChildren().get(0)));
    }

    @Test
    public void headingsWithoutLinksAreNotes() throws IOException {
        TreeNode<Link> root = parse("# heading with no link\n" +
                "* [child of heading with no link](bbbbbbb)\n" +
                "# heading [with a link](zzzzzzz)\n" +
                "* [child of heading with a link](aaaaaaa)\n");

        assertEquals(2, TreeViews.countChildren(root));

        TreeNode<Link> withoutLink = root.getChildren().get(0);
        assertEquals("heading with no link", TreeViews.getTitle(withoutLink));
        assertNull(TreeViews.getId(withoutLink));
        assertEquals(1, TreeViews.countChildren(withoutLink));

        TreeNode<Link> withALink = root.getChildren().get(1);
        assertEquals("with a link", TreeViews.getTitle(withALink));
        assertEquals("zzzzzzz", TreeViews.getId(withALink));
        assertEquals(1, TreeViews.countChildren(withALink));
        TreeNode<Link> child = withALink.getChildren().get(0);
        assertEquals("child of heading with a link", TreeViews.getTitle(child));
        assertEquals("aaaaaaa", TreeViews.getId(child));
    }

    @Test
    public void headingWithoutChildrenIsEquivalentToSimpleAtom() throws IOException {
        TreeNode<Link> root = parse("[simple atom](aaaaaaa)\n" +
                "\n" +
                "# heading without content\n" +
                "\n" +
                "# heading with content\n" +
                "\n" +
                "[another atom](bbbbbbb)");

        assertEquals(3, TreeViews.countChildren(root));

        TreeNode<Link> simpleAtom = root.getChildren().get(0);
        assertEquals("simple atom", TreeViews.getTitle(simpleAtom));
        assertEquals(0, TreeViews.countChildren(simpleAtom));
        TreeNode<Link> headingWithoutContent = root.getChildren().get(1);
        assertEquals("heading without content", TreeViews.getTitle(headingWithoutContent));
        assertEquals(0, TreeViews.countChildren(headingWithoutContent));
        TreeNode<Link> headingWithContent = root.getChildren().get(2);
        assertEquals("heading with content", TreeViews.getTitle(headingWithContent));
        assertEquals(1, TreeViews.countChildren(headingWithContent));
        TreeNode<Link> anotherAtom = headingWithContent.getChildren().get(0);
        assertEquals("another atom", TreeViews.getTitle(anotherAtom));
        assertEquals(0, TreeViews.countChildren(anotherAtom));
    }

    private TreeNode<Link> parse(final String content) throws IOException {
        ByteArrayInputStream input = new ByteArrayInputStream(content.getBytes());

        MarkdownParser parser = new MarkdownParser();
        //parser.setVerbose(true);
        return parser.parse(input).getContent();
    }
}
