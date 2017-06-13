package net.fortytwo.smsn.brain.io.pages;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.json.JsonFormat;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.Property;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.dto.LinkDTO;
import net.fortytwo.smsn.brain.model.dto.ListDTO;
import net.fortytwo.smsn.brain.model.dto.PageDTO;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.dto.TreeDTO;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.model.entities.EntityTree;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Stack;
import java.util.regex.Matcher;

public class PageParser {

    private enum State {Properties, Content, Text}

    private Page page;
    private Stack<Stack<EntityTree<Link>>> nodeHierarchy = new Stack<>();
    private Stack<Integer> indentHierarachy = new Stack<>();

    private int lineNumber;
    private String currentText;
    private String currentLine;
    private String currentLineTrimmed;
    private State currentState;

    public Page parse(final String input, String topicId, String label, String source) throws IOException {
        try (InputStream inputStream = new ByteArrayInputStream(input.getBytes())) {
            return parse(inputStream, topicId, label, source);
        }
    }

    public Page parse(final InputStream inputStream, String topicId, String label, String source)
            throws IOException {
        reset();
        createPage(topicId, label, source);

        BufferedReader br = createReader(inputStream);
        while ((currentLine = br.readLine()) != null) {
            parseLine();
        }

        finishText();

        adjustHierarchy(-1);

        return page;
    }

    private void createPage(String topicId, String label, String source) throws IOException {
        page = new PageDTO();
        page.setSource(source);
        nodeHierarchy.push(new Stack<>());
        EntityTree<Link> root = constructTreeNode(topicId, Role.Noun, label);
        page.setContent(root);
        nodeHierarchy.peek().push(root);
    }

    private void finishText() {
        if (State.Text == currentState && !isEmptyText(currentText)) {
            page.setText(currentText);
        }
    }

    private void parseLine() throws IOException {
        incrementLineNumber();

        replaceTabsWithSpaces();

        currentLineTrimmed = currentLine.trim();

        if (currentLineIsTextDelimiter()) {
            // note: the closing delimiter is optional, and redundant delimiters are ignored
            currentState = State.Text;
        } else {
            switch (currentState) {
                case Text:
                    if (!currentText.isEmpty()) currentText += "\n";
                    currentText += currentLine;
                    break;
                case Properties:
                    if (!currentLineIsEmpty()) {
                        if (currentLineIsProperty()) {
                            parsePropertyLine();
                            break;
                        } else {
                            // note: falls through to "Content" case
                            currentState = State.Content;
                        }
                    }
                case Content:
                    if (!currentLineIsEmpty()) {
                        validateContentLine();
                        parseContentLine();
                    }
                    break;
            }
        }
    }

    private boolean isEmptyText(final String page) {
        return page.trim().length() == 0;
    }

    private BufferedReader createReader(final InputStream in) throws IOException {
        return new BufferedReader(new InputStreamReader(in, SemanticSynchrony.UTF8));
    }

    private void reset() {
        nodeHierarchy.clear();
        indentHierarachy.clear();
        indentHierarachy.push(-1);

        currentState = State.Properties;
        currentText = "";
        lineNumber = 0;
    }

    private void replaceTabsWithSpaces() {
        // Tabs count as four spaces each.
        currentLine = currentLine.replaceAll("[\\t]", PageFormat.TAB_REPLACEMENT);
    }

    private void validateContentLine() throws IOException {
        if (currentLine.endsWith(JsonFormat.TITLE_TRUNCATOR)) {
            parseError("line ends with the reserved truncation sequence \"" + JsonFormat.TITLE_TRUNCATOR + "\"");
        }
    }

    private int findIndentLevel() {
        int level = 0;
        if (currentLine.length() > 0) {
            while (' ' == currentLine.charAt(level)) {
                level++;
            }
        }
        return level;
    }

    private void adjustHierarchy(final int indentLevel) {
        while (indentLevel < indentHierarachy.peek()) {
            indentHierarachy.pop();
            Stack<EntityTree<Link>> siblings = nodeHierarchy.pop();
            EntityTree<Link> parent = nodeHierarchy.peek().peek();
            addChildren(parent, siblings);
        }

        if (indentLevel > indentHierarachy.peek()) {
            indentHierarachy.push(indentLevel);
            nodeHierarchy.push(new Stack<>());
        }
    }

    private void addToHierarchy(final EntityTree<Link> tree) {
        adjustHierarchy(findIndentLevel());

        nodeHierarchy.peek().push(tree);
    }

    private void addChildren(final EntityTree<Link> parent, Stack<EntityTree<Link>> children) {
        if (children.isEmpty()) {
            return;
        }

        EntityList<EntityTree<Link>> cur = null;
        while (!children.isEmpty()) {
            cur = new ListDTO<>(children.pop(), cur);
        }
        parent.setChildren(cur);
    }

    private boolean currentLineIsEmpty() {
        return currentLineTrimmed.isEmpty();
    }

    private boolean currentLineIsProperty() {
        return currentLineTrimmed.startsWith("@");
    }

    private boolean currentLineIsTextDelimiter() {
        return PageFormat.TEXT_DELIMITER.equals(currentLineTrimmed);
    }

    private void validateLink(Link link) throws IOException {
        if (0 == link.getLabel().length()) {
            if (null == link.getTarget()) {
                parseError("empty label in placeholder link");
            } else {
                // Empty note values are allowed for existing notes.
                // They signify that an existing note's value should not be overwritten.
                link.setLabel(null);
            }
        }
    }

    private void parseError(final String message) throws IOException {
        throw new IOException("line " + lineNumber + ": " + message);
    }

    private void parsePropertyLine() throws IOException {
        String key;
        String value;

        int firstSpace = currentLineTrimmed.indexOf(' ');
        if (-1 == firstSpace) {
            key = currentLineTrimmed.substring(1);
            value = "";
        } else if (firstSpace < 2) {
            key = null;
            value = null;
            parseError("empty property key");
        } else {
            key = currentLineTrimmed.substring(1, firstSpace).trim();
            value = currentLineTrimmed.substring(firstSpace).trim();
        }

        checkForEmptyPropertyValue(key, value);

        setProperty(key, value);
    }

    private void checkForEmptyPropertyValue(final String key, final String value) throws IOException {
        if (value.isEmpty()) {
            // can "clear" alias or shortcut by writing "@alias" or "@shortcut" and nothing else;
            // all other properties require an argument
            if (!(key.equals("alias") || key.equals("shortcut"))) {
                parseError("empty value for property @" + key);
            }
        }
    }

    private void parseContentLine() throws IOException {
        int firstSpace = currentLineTrimmed.indexOf(' ');
        if (-1 == firstSpace) {
            parseError("missing item bullet");
        } else if (firstSpace > 2) {
            parseError("bullet is too long");
        }

        String bullet = currentLineTrimmed.substring(0, firstSpace);
        String rest = currentLineTrimmed.substring(firstSpace).trim();

        String id;
        String label;
        Matcher matcher = PageFormat.ID_INFIX.matcher(rest);
        if (matcher.find() && 0 == matcher.start()) {
            id = rest.substring(1, matcher.end() - 1);
            label = rest.substring(matcher.end()).trim();
        } else {
            id = null;
            label = rest;
        }

        EntityTree<Link> tree = constructTreeNode(id, roleForBullet(bullet), label);
        addToHierarchy(tree);
    }

    private void setProperty(final String key, String value) throws IOException {
        if (value.length() == 0) {
            value = Note.CLEARME;
        }

        Property<Page, Object> prop = Page.propertiesByKey.get(key);
        if (null == prop) {
            // unknown properties are quietly ignored
            return;
        }

        Object typeSafeValue;

        try {
            typeSafeValue = prop.getFromString().apply(value);
        } catch (Exception e) {
            parseError("invalid value for @" + key + " property: " + value);
            return;
        }

        prop.getSetter().accept(page, typeSafeValue);
    }

    private Role roleForBullet(final String bullet) {
        return bullet.equals(PageFormat.VERB_BULLET)
                ? Role.Verb
                : Role.Noun;
    }

    private EntityTree<Link> constructTreeNode(final String topicId, final Role role, final String label)
            throws IOException {
        Link link = new LinkDTO();
        link.setRole(role);
        link.setLabel(label);
        if (null != topicId) {
            Topic target = new TopicDTO();
            target.setId(topicId);
            link.setTarget(target);
        }

        validateLink(link);

        EntityTree<Link> treeNode = new TreeDTO<>();
        treeNode.setValue(link);
        return treeNode;
    }

    private void incrementLineNumber() {
        lineNumber++;
    }
}
