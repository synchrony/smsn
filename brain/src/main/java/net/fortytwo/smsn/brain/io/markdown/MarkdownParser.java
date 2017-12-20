package net.fortytwo.smsn.brain.io.markdown;

import net.fortytwo.smsn.brain.io.NoteParser;
import net.fortytwo.smsn.brain.model.entities.Note;
import org.commonmark.node.Document;
import org.commonmark.node.Node;
import org.commonmark.parser.Parser;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public class MarkdownParser extends NoteParser {
    private boolean verbose;

    public void setVerbose(boolean verbose) {
        this.verbose = verbose;
    }

    @Override
    public Note parse(InputStream inputStream) throws IOException {
        Document document = parseToMarkdownDocument(inputStream);
        return parse(document);
    }

    private Document parseToMarkdownDocument(final InputStream input) throws IOException {
        Parser parser = Parser.builder().build();
        Node document = parser.parseReader(new InputStreamReader(input));
        return (Document) document;
    }

    private Note parse(final Document document) {
        MarkdownVisitor visitor = new MarkdownVisitor(verbose);
        document.accept(visitor);
        return visitor.getRoot();
    }
}
