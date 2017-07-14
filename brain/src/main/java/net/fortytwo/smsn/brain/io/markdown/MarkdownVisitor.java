package net.fortytwo.smsn.brain.io.markdown;

import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import org.commonmark.node.BlockQuote;
import org.commonmark.node.BulletList;
import org.commonmark.node.Code;
import org.commonmark.node.CustomBlock;
import org.commonmark.node.CustomNode;
import org.commonmark.node.Document;
import org.commonmark.node.Emphasis;
import org.commonmark.node.FencedCodeBlock;
import org.commonmark.node.HardLineBreak;
import org.commonmark.node.Heading;
import org.commonmark.node.HtmlBlock;
import org.commonmark.node.HtmlInline;
import org.commonmark.node.Image;
import org.commonmark.node.IndentedCodeBlock;
import org.commonmark.node.Link;
import org.commonmark.node.ListItem;
import org.commonmark.node.Node;
import org.commonmark.node.OrderedList;
import org.commonmark.node.Paragraph;
import org.commonmark.node.SoftLineBreak;
import org.commonmark.node.StrongEmphasis;
import org.commonmark.node.Text;
import org.commonmark.node.ThematicBreak;
import org.commonmark.node.Visitor;

import java.util.Stack;

class MarkdownVisitor implements Visitor {

    private final Stack<TreeNode<net.fortytwo.smsn.brain.model.entities.Link>> headings = new Stack<>();
    private int currentDepth;
    private final TreeNode<net.fortytwo.smsn.brain.model.entities.Link> root;
    private final boolean verbose;

    public TreeNode<net.fortytwo.smsn.brain.model.entities.Link> getRoot() {
        return root;
    }

    public MarkdownVisitor(final boolean verbose) {
        this.verbose = verbose;

        headings.clear();
        root = TreeNodeDTO.createEmptyNode();
        addHeading(root, 0);
        currentDepth = 0;
    }

    @Override
    public void visit(BulletList bulletList) {
        visitChildren(bulletList);
    }

    @Override
    public void visit(Heading heading) {
        addHeading(toTree(heading), heading.getLevel());
    }

    @Override
    public void visit(Link link) {
        String title;
        String destination = link.getDestination();

        if (null != destination && 0 < destination.length() && null != (title = getLinkTitle(link))) {
            TreeNode<net.fortytwo.smsn.brain.model.entities.Link> tree = TreeNodeDTO.createEmptyNode();
            TreeViews.setId(tree, link.getDestination());
            TreeViews.setTitle(tree, title);

            addNote(tree);
        }
    }

    @Override
    public void visit(ListItem listItem) {
        visitChildren(listItem);
    }

    @Override
    public void visit(OrderedList orderedList) {
        visitChildren(orderedList);
    }

    @Override
    public void visit(Paragraph paragraph) {
        visitChildren(paragraph);
    }

    @Override
    public void visit(BlockQuote blockQuote) {
        visitChildren(blockQuote);
    }

    @Override
    public void visit(Code code) {
        visitChildren(code);
    }

    @Override
    public void visit(Document document) {
        visitChildren(document);
    }

    @Override
    public void visit(Emphasis emphasis) {
        visitChildren(emphasis);
    }

    @Override
    public void visit(FencedCodeBlock fencedCodeBlock) {
        visitChildren(fencedCodeBlock);
    }

    @Override
    public void visit(HardLineBreak hardLineBreak) {
        visitChildren(hardLineBreak);
    }

    @Override
    public void visit(ThematicBreak thematicBreak) {
        visitChildren(thematicBreak);
    }

    @Override
    public void visit(HtmlInline htmlInline) {
        visitChildren(htmlInline);
    }

    @Override
    public void visit(HtmlBlock htmlBlock) {
        visitChildren(htmlBlock);
    }

    @Override
    public void visit(Image image) {
        visitChildren(image);
    }

    @Override
    public void visit(IndentedCodeBlock indentedCodeBlock) {
        visitChildren(indentedCodeBlock);
    }

    @Override
    public void visit(SoftLineBreak softLineBreak) {
        visitChildren(softLineBreak);
    }

    @Override
    public void visit(StrongEmphasis strongEmphasis) {
        visitChildren(strongEmphasis);
    }

    @Override
    public void visit(Text text) {
        visitChildren(text);
    }

    @Override
    public void visit(CustomBlock customBlock) {
        visitChildren(customBlock);
    }

    @Override
    public void visit(CustomNode customNode) {
        visitChildren(customNode);
    }

    private TreeNode<net.fortytwo.smsn.brain.model.entities.Link> toTree(final Heading heading) {
        TreeNode<net.fortytwo.smsn.brain.model.entities.Link> tree = TreeNodeDTO.createEmptyNode();

        Node child = heading.getFirstChild();
        while (null != child) {
            if (child instanceof Link) {
                Link link = (Link) child;
                String title = getLinkTitle(link);
                if (null != title && null != link.getDestination() && 0 < link.getDestination().length()) {
                    TreeViews.setId(tree, link.getDestination().trim());
                    TreeViews.setTitle(tree, title);
                    break;
                }
            } else if (child instanceof Text) {
                Text text = (Text) child;
                TreeViews.setTitle(tree, text.getLiteral().trim());
            }
            child = child.getNext();
        }

        return tree;
    }

    private String getLinkTitle(final Link link) {
        String title;
        if (null != link.getTitle() && 0 < (title = link.getTitle().trim()).length()) {
            return title;
        } else {
            Node text = link.getFirstChild();
            if (null != text && text instanceof Text) {
                title = ((Text) text).getLiteral().trim();
                return title.length() > 0 ? title : null;
            } else {
                return null;
            }
        }
    }

    private void addNote(final TreeNode<net.fortytwo.smsn.brain.model.entities.Link> note) {
        if (!headings.isEmpty()) {
            headings.peek().addChild(note);
        }
    }

    private void addHeading(final TreeNode<net.fortytwo.smsn.brain.model.entities.Link> note, int level) {
        if (level > headings.size()) {
            throw new IllegalStateException("invalid heading level (" + level + " > " + headings.size() + ")");
        }

        while (headings.size() > level) {
            headings.pop();
        }

        addNote(note);

        headings.push(note);
    }

    private void visitChildren(final Node node) {
        if (verbose) {
            echo(node);
        }

        currentDepth++;
        Node child = node.getFirstChild();
        while (null != child) {
            child.accept(this);
            child = child.getNext();
        }
        currentDepth--;
    }

    private void echo(final Node node) {
        System.out.println(indentByDocumentDepth() + node);
    }

    private String indentByDocumentDepth() {
        String s = "";
        for (int i = 0; i < currentDepth; i++) s += "    ";
        return s;
    }
}
