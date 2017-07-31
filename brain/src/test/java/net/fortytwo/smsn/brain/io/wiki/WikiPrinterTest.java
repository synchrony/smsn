package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.dto.LinkDTO;
import net.fortytwo.smsn.brain.model.dto.ListNodeDTO;
import net.fortytwo.smsn.brain.model.dto.PageDTO;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;

import static org.junit.Assert.assertEquals;

public class WikiPrinterTest {
    private WikiPrinter printer;
    private ByteArrayOutputStream outputStream;

    @Before
    public void setUp() {
        outputStream = new ByteArrayOutputStream();
        printer = new WikiPrinter(outputStream);
    }

    @Test
    public void propertiesPrecedeContent() throws Exception {
        Page page = new PageDTO();
        page.setContent(createTree("123", "Arthur Dent", null));
        page.setAlias("http://example.org/ArthurDent");
        page.setPriority(1.0f);
        page.setShortcut("ad");

        assertEquals("@id 123\n" +
                "@title Arthur Dent\n" +
                "@alias http://example.org/ArthurDent\n"
                + "@shortcut ad\n"
                + "@priority 1.0\n", write(page));
    }

    @Test
    public void textFollowsContent() throws Exception {
        Page page = new PageDTO();
        page.setContent(createTree("12345", "Arthur Dent", null));
        page.setText("The regular early morning yell of horror was the sound of Arthur Dent waking\n" +
                "up and suddenly remembering where he was.");

        assertEquals("@id 12345\n" +
                "@title Arthur Dent\n" +
                "@text ```\n" +
                "The regular early morning yell of horror was the sound of Arthur Dent waking\n" +
                "up and suddenly remembering where he was.\n" +
                "```\n", write(page));
    }

    @Test
    public void textIsOptional() throws Exception {
        Page page = new PageDTO();
        page.setContent(createTree("12345", "Arthur Dent", null));
        page.setText(null);

        assertEquals("@id 12345\n" +
                "@title Arthur Dent\n", write(page));
    }

    @Test
    public void emptyTextIsIgnored() throws Exception {
        Page page = new PageDTO();
        page.setContent(createTree("12345", "Arthur Dent", null));
        page.setText("  \n ");

        assertEquals("@id 12345\n" +
                "@title Arthur Dent\n", write(page));
    }

    @Test
    public void trailingTextWhitespaceIsIgnored() throws Exception {
        Page page = new PageDTO();
        page.setContent(createTree("12345", "Arthur Dent", null));
        page.setText("one\ntwo\n\n  \t\n ");

        assertEquals("@id 12345\n" +
                "@title Arthur Dent\n" +
                "@text ```\n" +
                "one\n" +
                "two\n" +
                "```\n", write(page));
    }

    @Test
    public void nounAndVerbBulletsAreDistinguished() throws Exception {
        Page page = new PageDTO();
        page.setContent(createTree("123", "Arthur Dent", null));
        TreeNode<Link> content = page.getContent();
        TreeNode<Link> node1 = createTree(null, "Arthur Philip Dent", null);
        TreeNode<Link> node2 = createTree("12345", "friends", Role.Relation);
        TreeNode<Link> node3 = createTree("00000", "Ford Prefect", null);
        TreeNode<Link> node4 = createTree(null, "Slartibartfast", null);
        content.setChildren(ListNodeDTO.fromArray(node1, node2));
        node2.setChildren(ListNodeDTO.fromArray(node3, node4));

        assertEquals("@id 123\n" +
                "@title Arthur Dent\n" +
                "* Arthur Philip Dent\n" +
                "-- :12345: friends\n" +
                "    * :00000: Ford Prefect\n" +
                "    * Slartibartfast\n", write(page));
    }

    private TreeNode<Link> createTree(final String id, final String label, final Role role) {
        Link link = new LinkDTO();
        link.setRole(role);
        link.setLabel(label);
        if (null != id) {
            Topic topic = new TopicDTO();
            topic.setId(id);
            link.setTarget(topic);
        }
        TreeNode<Link> tree = new TreeNodeDTO<>();
        tree.setValue(link);
        return tree;
    }

    private String write(final Page page) {
        printer.print(page);
        return outputStream.toString();
    }
}
