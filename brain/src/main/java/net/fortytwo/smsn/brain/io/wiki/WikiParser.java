package net.fortytwo.smsn.brain.io.wiki;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.BrainParser;
import net.fortytwo.smsn.brain.io.json.JsonFormat;
import net.fortytwo.smsn.brain.model.Note;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.regex.Matcher;

public class WikiParser extends BrainParser {

    private Note root;
    private LinkedList<Note> hierarchy;
    private LinkedList<Integer> indentHierarachy;
    private boolean useCanonicalFormat;

    private int lineNumber;
    private String currentValue;
    private String currentLine;
    private boolean currentLineIsProperty;
    private int currentIndentLevel;
    private String currentId;
    private String currentBullet;
    private boolean currentLineIsInPage;

    public void setUseCanonicalFormat(boolean useCanonicalFormat) {
        this.useCanonicalFormat = useCanonicalFormat;
    }

    @Override
    public Note parse(final InputStream inputStream) throws IOException {
        reset();
        BufferedReader bufferedReader = createReader(inputStream);

        while ((currentLine = bufferedReader.readLine()) != null) {
            parseLine();
        }

        if (currentLineIsInPage && !isEmptyPage(currentValue)) {
            root.getChildren().get(0).setPage(currentValue);
        }

        return root;
    }

    private boolean isEmptyPage(final String page) {
        return page.trim().length() == 0;
    }

    private BufferedReader createReader(final InputStream in) throws IOException {
        return new BufferedReader(new InputStreamReader(in, SemanticSynchrony.UTF8));
    }

    private void reset() {
        root = new Note();
        hierarchy = new LinkedList<>();
        indentHierarachy = new LinkedList<>();
        lineNumber = 0;
        currentLineIsInPage = false;
    }

    private void replaceTabsWithSpaces() {
        // Tabs count as four spaces each.
        currentLine = currentLine.replaceAll("[\\t]", WikiFormat.TAB_REPLACEMENT);
    }

    private void validateLine() throws IOException {
        if (currentLine.endsWith(JsonFormat.TITLE_TRUNCATOR)) {
            parseError("line ends with the reserved truncation sequence \"" + JsonFormat.TITLE_TRUNCATOR + "\"");
        }
    }

    private void findIndentLevel() {
        currentIndentLevel = 0;
        if (currentLine.length() > 0) {
            while (' ' == currentLine.charAt(currentIndentLevel)) {
                currentIndentLevel++;
            }
            currentLine = currentLine.substring(currentIndentLevel);
        }
    }

    private void updateHierarchy() {
        while (0 < hierarchy.size() && indentHierarachy.getLast() >= currentIndentLevel) {
            hierarchy.removeLast();
            indentHierarachy.removeLast();
        }
    }

    private boolean lineIsEmpty() {
        return 0 == currentLine.trim().length();
    }

    private void checkForEmptyValue() throws IOException {
        if (0 == currentValue.length()) {
            if (currentLineIsProperty) {
                // can "clear" alias or shortcut by writing "@alias" or "@shortcut" and nothing else;
                // all other properties require an argument
                if (!(currentBullet.equals("@alias") || currentBullet.equals("@shortcut"))) {
                    parseError("empty value for property candidate '" + currentBullet + "'");
                }
            } else if (null == currentId) {
                parseError("empty value for new note");
            } else {
                // Empty note values are allowed for existing notes.
                // They signify that an existing note's value should not be overwritten.
                currentValue = null;
            }
        }
    }

    private void parseError(final String message) throws IOException {
        throw new IOException("line " + lineNumber + ": " + message);
    }

    private void parseBulletOrPropertyName() throws IOException {
        // parse bullet or property name
        int j = -1;
        for (int i = 0; i < currentLine.length(); i++) {
            char c = currentLine.charAt(i);
            if (' ' == c) {
                j = i;
                break;
            }
        }
        if (j < 0) {
            j = currentLine.length();
        }

        currentBullet = currentLine.substring(0, j);
        if (currentBullet.startsWith("@") && currentBullet.length() > 1) {
            currentLineIsProperty = true;
        } else {
            currentLineIsProperty = false;

            if (currentBullet.length() > WikiFormat.MAX_BULLET_LENGTH) {
                parseError("bullet is too long: " + currentBullet);
            }
        }

        // skip white space between bullet and value
        while (j < currentLine.length() && ' ' == currentLine.charAt(j)) {
            j++;
        }
        currentLine = currentLine.substring(j);
    }

    private void parseId() {
        currentId = null;
        if (!currentLineIsProperty) {
            Matcher m = WikiFormat.ID_INFIX.matcher(currentLine);
            if (m.find() && 0 == m.start()) {
                currentId = currentLine.substring(1, m.end() - 1);
                currentLine = currentLine.substring(m.end()).trim();
            }
        }
    }

    private void constructNote() throws IOException {
        checkForEmptyValue();

        if (currentLineIsProperty) {
            Note n = 0 == hierarchy.size() ? root : hierarchy.get(hierarchy.size() - 1);

            String key = currentBullet.substring(1);
            parseProperty(n, key, currentValue);
        } else {
            Note n = new Note();
            n.setTitle(currentValue);

            n.setId(currentId);

            if (0 < hierarchy.size()) {
                hierarchy.get(hierarchy.size() - 1).addChild(n);
            } else {
                root.addChild(n);
            }

            hierarchy.add(n);
            indentHierarachy.add(currentIndentLevel);
        }
    }

    private void parseLine() throws IOException {
        incrementLineNumber();

        replaceTabsWithSpaces();

        if (currentLineIsInPage) {
            if (!currentValue.isEmpty()) currentValue += "\n";
            currentValue += currentLine;
        } else {
            if (lineIsEmpty()) {
                if (useCanonicalFormat) {
                    currentLineIsInPage = true;
                    currentValue = "";
                }
            } else {
                parseStructuredLine();
            }
        }
    }

    private void parseStructuredLine() throws IOException {
        findIndentLevel();

        updateHierarchy();

        parseBulletOrPropertyName();

        parseId();

        validateLine();

        parseNormalValue();

        constructNote();
    }

    private void parseNormalValue() throws IOException {
        currentValue = "";
        if (0 == currentLine.length()) return;

        currentValue = currentLine.trim();
    }

    private void incrementLineNumber() {
        lineNumber++;
    }

    private void parseProperty(final Note note, final String key, String rawValue)
            throws IOException {

        if (rawValue.length() == 0) {
            rawValue = Note.CLEARME;
        }

        Note.Property prop = Note.propertiesByKey.get(key);
        if (null == prop) {
            return;
        }

        Object typeSafeValue;

        try {
            typeSafeValue = prop.getFromString().apply(rawValue);
        } catch (Exception e) {
            parseError("invalid @" + key + " value: " + rawValue);
            return;
        }

        prop.getNoteSetter().accept(note, typeSafeValue);
    }
}
