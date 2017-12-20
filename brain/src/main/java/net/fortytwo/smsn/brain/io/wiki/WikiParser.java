package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.NoteParser;
import net.fortytwo.smsn.brain.io.json.JsonFormat;
import net.fortytwo.smsn.brain.model.Property;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Stack;
import java.util.regex.Matcher;

public class WikiParser extends NoteParser {

    private enum State {Properties, Content, Text}

    private Note root;
    private final Stack<Stack<Note>> noteHierarchy = new Stack<>();
    private final Stack<Integer> indentHierarachy = new Stack<>();

    private int lineNumber;
    private String currentLine;
    private String currentLineTrimmed;
    private State currentState;

    private String currentPropertyKey;
    private String currentPropertyValue;
    private Note currentNote;

    @Override
    public Note parse(final InputStream inputStream) throws IOException {
        return parseInternal(inputStream);
    }

    private Note parseInternal(final InputStream inputStream) throws IOException {
        reset();

        BufferedReader br = createReader(inputStream);
        while ((currentLine = br.readLine()) != null) {
            parseLine();
        }

        adjustHierarchy(-1);

        return root;
    }
    
    private void parseLine() throws IOException {
        incrementLineNumber();

        replaceTabsWithSpaces();

        currentLineTrimmed = currentLine.trim();

        switch (currentState) {
            case Properties:
                if (!currentLineIsEmpty()) {
                    if (currentLineIsProperty()) {
                        parsePropertyLine(root);
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
            case Text:
                if (currentLineTrimmed.equals(WikiFormat.MULTILINE_DELIMITER)) {
                    currentState = State.Properties;
                    finishProperty();
                } else {
                    if (!currentPropertyValue.isEmpty()) currentPropertyValue += "\n";
                    currentPropertyValue += currentLine;
                }
                break;
        }
    }

    private BufferedReader createReader(final InputStream in) throws IOException {
        return new BufferedReader(new InputStreamReader(in, SemanticSynchrony.UTF8));
    }

    private void reset() {
        noteHierarchy.clear();
        indentHierarachy.clear();
        indentHierarachy.push(-1);

        currentState = State.Properties;
        lineNumber = 0;

        root = new NoteDTO();
        noteHierarchy.push(new Stack<>());
        noteHierarchy.peek().push(root);
    }

    private void replaceTabsWithSpaces() {
        // Tabs count as four spaces each.
        currentLine = currentLine.replaceAll("[\\t]", WikiFormat.TAB_REPLACEMENT);
    }

    private void validateContentLine() throws IOException {
        if (currentLine.endsWith(JsonFormat.TITLE_TRUNCATOR)) {
            parseError("line ends with the reserved truncation sequence \"" + JsonFormat.TITLE_TRUNCATOR + "\"");
        }
    }

    private int findIndentLevel() {
        int level = 0;
        if (null != currentLine && currentLine.length() > 0) {
            while (' ' == currentLine.charAt(level)) {
                level++;
            }
        }
        return level;
    }

    private void adjustHierarchy(final int indentLevel) {
        while (indentLevel < indentHierarachy.peek()) {
            indentHierarachy.pop();
            Stack<Note> siblings = noteHierarchy.pop();
            Note parent = noteHierarchy.peek().peek();
            Note.setChildren(parent, siblings);
        }

        if (indentLevel > indentHierarachy.peek()) {
            indentHierarachy.push(indentLevel);
            noteHierarchy.push(new Stack<>());
        }
    }

    private Note getCurrentNote() {
        return noteHierarchy.peek().peek();
    }

    private void addToHierarchy(final Note note) {
        adjustHierarchy(findIndentLevel());

        noteHierarchy.peek().push(note);
    }

    private boolean currentLineIsEmpty() {
        return currentLineTrimmed.isEmpty();
    }

    private boolean currentLineIsProperty() {
        return currentLineTrimmed.startsWith("@");
    }

    private void validate(Note note) throws IOException {
        String label = note.getLabel();
        if (null != label && 0 == label.length()) {
            if (null == note.getTopic()) {
                parseError("empty label in placeholder note");
            } else {
                // Empty labels are allowed for existing links.
                // They signify that an existing link's label should not be overwritten.
                note.setLabel(null);
            }
        }
    }

    private void parseError(final String message) throws IOException {
        throw new IOException("line " + lineNumber + ": " + message);
    }

    private void parsePropertyLine(final Note note) throws IOException {
        currentNote = note;

        int firstSpace = currentLineTrimmed.indexOf(' ');
        if (-1 == firstSpace) {
            currentPropertyKey = currentLineTrimmed.substring(1);
            currentPropertyValue = "";
        } else if (firstSpace < 2) {
            parseError("empty property key");
        } else {
            currentPropertyKey = currentLineTrimmed.substring(1, firstSpace).trim();
            String value = currentLineTrimmed.substring(firstSpace).trim();
            if (value.equals(WikiFormat.MULTILINE_DELIMITER)) {
                currentPropertyValue = "";
                currentState = State.Text;
                return;
            } else {
                currentPropertyValue = value;
            }
        }

        finishProperty();
    }

    private void finishProperty() throws IOException {
        String key = currentPropertyKey;
        String value = WikiFormat.stripTrailingSpace(currentPropertyValue);

        checkForEmptyPropertyValue(key, value);
        setProperty(currentNote, key, value);
    }

    private void checkForEmptyPropertyValue(final String key, final String value) throws IOException {
        if (value.isEmpty()) {
            // can "clear" alias or shortcut by writing "@alias" or "@shortcut" and nothing else;
            // all other properties require an argument
            if (!(key.equals("alias") || key.equals("shortcut") || key.equals("text"))) {
                parseError("empty value for property @" + key);
            }
        }
    }

    private void parseContentLine() throws IOException {
        if (currentLineTrimmed.startsWith("@")) {
            parseContentPropertyLine();
        } else {
            parseContentTitleLine();
        }
    }

    private void parseContentPropertyLine() throws IOException {
        parsePropertyLine(getCurrentNote());
    }

    private void parseContentTitleLine() throws IOException {
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
        Matcher matcher = WikiFormat.ID_INFIX.matcher(rest);
        if (matcher.find() && 0 == matcher.start()) {
            id = rest.substring(1, matcher.end() - 1);
            label = rest.substring(matcher.end()).trim();
        } else {
            id = null;
            label = rest;
        }

        Note note = constructNote(id, tagForBullet(bullet), label);
        addToHierarchy(note);
    }

    private <T> void setProperty(final Note note, final String key, String value) throws IOException {
        if (value.length() == 0) {
            value = WikiFormat.CLEARME;
        }

        // TODO: transitional
        switch (key) {
            case "id":
                note.getTopic().setId(value);
                break;
            case "title":
                note.setLabel(value);
                break;
            default:
                Property<Note, T> prop = (Property<Note, T>) Note.propertiesByKey.get(key);
                if (null == prop) {
                    // unknown properties are quietly ignored
                    return;
                }

                T typeSafeValue;

                try {
                    typeSafeValue = prop.getFromString().apply(value);
                } catch (Exception e) {
                    parseError("invalid value for @" + key + " property: " + value);
                    return;
                }

                prop.getSetter().accept(note, typeSafeValue);
                break;
        }
    }

    private Role tagForBullet(final String bullet) {
        return bullet.equals(WikiFormat.LABEL_BULLET)
                ? Role.Relation
                : null;
    }

    private Note constructNote(final String topicId, final Role role, final String label)
            throws IOException {
        Note note = new NoteDTO();
        note.setRole(role);
        note.setLabel(label);
        if (null != topicId) {
            Topic topic = new TopicDTO();
            topic.setId(topicId);
            note.setTopic(topic);
        }
        validate(note);
        return note;
    }

    private void incrementLineNumber() {
        lineNumber++;
    }
}
