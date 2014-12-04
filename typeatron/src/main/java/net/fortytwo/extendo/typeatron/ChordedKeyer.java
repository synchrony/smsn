package net.fortytwo.extendo.typeatron;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

/**
 * A recognizer of the Typeatron's five-key chording scheme
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ChordedKeyer {

    private byte[] lastInput;

    public enum Mode {
        TextEdit, CommandLine, Arrows, Mash;
    }

    public enum Modifier {Control, None}

    public enum SpecialChar {DEL, ESC, up, down, left, right}

    private class StateNode {
        /**
         * A symbol emitted when this state is reached
         */
        public String symbol;

        /**
         * A mode entered when this state is reached
         */
        public Mode mode;

        /**
         * A modifier applied when this state is reached
         */
        public Modifier modifier;

        public StateNode[] nextNodes = new StateNode[5];
    }

    private Map<Mode, StateNode> rootStates;

    private Mode currentMode;
    private StateNode currentButtonState;
    private int totalButtonsCurrentlyPressed;

    private final EventHandler eventHandler;

    private Map<String, String> punctuationMap;

    public ChordedKeyer(final EventHandler eventHandler) throws IOException {
        this.eventHandler = eventHandler;
        initializeChords();
    }

    /**
     * Processes the next input state
     *
     * @param state the input state, represented by a 4-byte sequence of '0's and '1's
     */
    // TODO: do away with the inefficient format, if it doesn't complicate things in Max/MSP
    public void nextInputState(final byte[] state) {
        for (int i = 0; i < 5; i++) {
            // Generally, at most one button should change per time step
            // However, if two buttons change state, it is an arbitrary choice w.r.t. which one changed first
            if (state[i] != lastInput[i]) {
                if ('1' == state[i]) {
                    buttonPressed(i);
                } else {
                    buttonReleased(i);
                }
            }
        }

        System.arraycopy(state, 0, lastInput, 0, 5);
    }

    public Map<String, String> getPunctuationMap() {
        return punctuationMap;
    }

    private void addChord(final Mode[] inputModes,
                          final String sequence,
                          final Mode outputMode,
                          final Modifier outputModifier,
                          final String outputSymbol) {
        for (Mode m : inputModes) {
            addChord(m, sequence, outputMode, outputModifier, outputSymbol);
        }
    }

    private void addChord(final Mode inputMode,
                          final String sequence,
                          final Mode outputMode,
                          final Modifier outputModifier,
                          final String outputSymbol) {
        StateNode cur = rootStates.get(inputMode);
        int l = sequence.length();
        for (int j = 0; j < l; j++) {
            int index = sequence.charAt(j) - 49;
            StateNode next = cur.nextNodes[index];
            if (null == next) {
                next = new StateNode();
                cur.nextNodes[index] = next;
            }

            cur = next;
        }

        if (null != outputSymbol) {
            if (null != cur.symbol && !cur.symbol.equals(outputSymbol)) {
                throw new IllegalStateException("conflicting symbols ('"
                        + cur.symbol + "' vs. '" + outputSymbol + "') for sequence " + sequence);
            }
            cur.symbol = outputSymbol;
        }

        if (null != outputMode) {
            if (null != cur.mode && cur.mode != outputMode) {
                throw new IllegalArgumentException("conflicting output modes ("
                        + cur.mode + " vs. " + outputMode + ") for sequence " + sequence);
            }
            cur.mode = outputMode;
        }

        if (null != outputModifier) {
            if (null != cur.modifier && cur.modifier != outputModifier) {
                throw new IllegalArgumentException("conflicting output modifiers ("
                        + cur.modifier + " vs. " + outputModifier + ") for sequence " + sequence);
            }

            cur.modifier = outputModifier;
        }

        if (null != cur.mode && null != cur.symbol) {
            throw new IllegalStateException(
                    "sequence has been assigned both an output symbol and an output mode: " + sequence);
        }
    }

    private void initializeChords() throws IOException {
        // TODO: we shouldn't assume the device powers up with no buttons pressed, although this is likely
        totalButtonsCurrentlyPressed = 0;
        lastInput = "00000".getBytes();

        rootStates = new HashMap<Mode, StateNode>();
        for (Mode m : Mode.values()) {
            rootStates.put(m, new StateNode());
        }

        currentMode = Mode.CommandLine;
        currentButtonState = rootStates.get(currentMode);

        // control-space codes for the Typeatron dictionary operator
        addChord(Mode.CommandLine, "11", null, Modifier.Control, "");
        // toggle text entry modes
        addChord(Mode.CommandLine, "55", Mode.TextEdit, null, null);

        addChord(Mode.TextEdit, "1441", Mode.CommandLine, null, null);
        addChord(Mode.TextEdit, "55", null, null, SpecialChar.ESC.name());
        // toggle text entry modes
        addChord(Mode.TextEdit, "11", Mode.CommandLine, null, null);

        // this completely describes Arrow mode
        addChord(Mode.TextEdit, "1221", Mode.Arrows, null, null);
        addChord(Mode.Arrows, "22", null, null, SpecialChar.right.name());
        addChord(Mode.Arrows, "33", null, null, SpecialChar.left.name());
        addChord(Mode.Arrows, "44", null, null, SpecialChar.up.name());
        addChord(Mode.Arrows, "55", null, null, SpecialChar.down.name());
        addChord(Mode.Arrows, "11", Mode.TextEdit, null, null);

        // TODO: restore these... maybe
        /*
        addChord(Mode.Text, "1212", null, Modifier.Control, "u"); // uppercase text
        addChord(Mode.Text, "1313", null, Modifier.Control, "p"); // punctuation
        addChord(Mode.Text, "1414", null, Modifier.Control, "n"); // numbers
        */

        // TODO: restore mash mode
        /*
        // control-ESC codes for dictionary put
        addChord(Mode.Text, "1221", null, Modifier.Control, "ESC");
        // control-DEL codes for dictionary get
        addChord(Mode.Text, "1212", null, Modifier.Control, "DEL");
        //addChord(Mode.Text, "1221", null, Modifier.Control, null);
        //addChord(Mode.Text, "1212", Mode.Hardware, null, null);
        // 1331 unassigned
        addChord(Mode.Text, "1313", Mode.Numeric, null, null);
        // 1441 unassigned
        addChord(Mode.Text, "1414", Mode.Text, null, null);  // a no-op
        addChord(Mode.Text, "1551", Mode.Mash, Modifier.None, null);
        // 1515 unassigned
        */

        /*
        // any keypress both activates the laser, then upon release terminates laser mode
        for (int i = 1; i <= 5; i++) {
            addChord(Mode.Laser, "" + i + i, Mode.Text, Modifier.None, null);
        }*/

        // return to default mode from anywhere other than mash mode
        for (Mode m : Mode.values()) {
            if (m != Mode.Mash) {
                addChord(m, "123321", Mode.TextEdit, Modifier.None, null);
            }
        }

        Mode[] textEntryModes = new Mode[]{Mode.TextEdit, Mode.CommandLine};

        // return from mash mode
        addChord(Mode.Mash, "1234554321", Mode.TextEdit, Modifier.None, null);

        // newline, space, delete available in all of the text-entry modes
        for (Mode m : textEntryModes) {
            // the "trigger finger" chord has a "do"/"execute" function in various contexts
            addChord(m, "22", null, null, "\n");

            addChord(m, "33", null, null, " ");
            addChord(m, "44", null, null, SpecialChar.DEL.name());
        }

        punctuationMap = new HashMap<String, String>();
        InputStream in = TypeatronControl.class.getResourceAsStream("typeatron-letters-and-punctuation.csv");
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            String line;
            while (null != (line = br.readLine())) {
                line = line.trim();
                if (line.length() > 0 && !line.startsWith("#")) {
                    String[] a = line.split(",");
                    String chord = a[0];

                    String letter = a[1].trim();
                    addChord(textEntryModes, chord, null, null, letter);
                    if (chord.length() == 2*2) {
                        addChord(textEntryModes, findControlChord(chord), null, Modifier.Control, letter);
                    }

                    if (a.length >= 3) {
                        String capital = a[2].trim();
                        if (capital.length() > 0) {
                            addChord(textEntryModes, findShiftChord(chord), null, null, capital);
                            addChord(textEntryModes, findControlShiftChord(chord), null, Modifier.Control, capital);
                        }
                    }

                    if (a.length >= 4) {
                        String punc = a[3].trim().replaceAll("comma", ",");
                        if (punc.length() > 0 && chord.length() < 3 * 2) {
                            punctuationMap.put(letter, punc);
                            addChord(textEntryModes, findPunctuationChord(chord), null, null, punc);

                            // note: we don't bother with control-punctuation chords for now,
                            // but they are possible
                            addChord(textEntryModes, findControlChord(findPunctuationChord(chord)), null, Modifier.Control, punc);
                        }
                    }

                    if (a.length >= 5) {
                        String symbol = a[4].trim();
                        if (symbol.length() > 0) {
                            addChord(textEntryModes, findExtendedCharacterChord(chord), null, null, symbol);
                        }
                    }
                }
            }
        } finally {
            in.close();
        }
    }

    private char findUnusedKey(final String chord,
                               final int index) {
        int ix = index;
        boolean[] used = new boolean[5];
        for (byte b : chord.getBytes()) {
            used[b - 49] = true;
        }

        for (int i = 0; i < 5; i++) {
            if (!used[i]) {
                if (0 == ix) {
                    return (char) (i + 49);
                } else {
                    ix--;
                }
            }
        }

        throw new IllegalArgumentException("index " + index + " too high for chord " + chord);
    }

    private String findControlChord(final String chord) {
        if (4 > chord.length()) {
            throw new IllegalStateException("can't control-modify a chord of length less than 4");
        }

        // we add a "flourish" to the second key pressed
        char key = chord.charAt(1);

        return chord.substring(0, 2) + key + key + chord.substring(2);
    }

    private String findShiftChord(final String chord) {
        if (4 != chord.length()) {
            throw new IllegalStateException("can only create shift chords for 2-key combos at present");
        }

        // we add a "flourish" to the first key pressed
        char key = chord.charAt(0);

        return chord.substring(0, 2) + key + key + chord.substring(2);
    }

    // note: shift-control chords, with some other meaning, are also possible
    private String findControlShiftChord(final String chord) {
        if (4 != chord.length()) {
            throw new IllegalStateException("can only create control-shift chords for 2-key combos at present");
        }

        // we add "flourishes" to the second key pressed and the first key released
        char key1 = chord.charAt(1);
        char key2 = chord.charAt(0);

        return chord.substring(0, 2) + key1 + key1 + key2 + key2 + chord.substring(2);
    }

    private String findPunctuationChord(final String chord) {
        char key = findUnusedKey(chord, 0);
        // the extra key is pressed and released "in the middle" of the modified chord
        return chord.substring(0, 2) + key + key + chord.substring(2);
    }

    private String findExtendedCharacterChord(final String chord) {
        char key = findUnusedKey(chord, 2);
        // the extra key is pressed and released "in the middle" of the modified chord
        return chord.substring(0, 2) + key + key + chord.substring(2);
    }

    // buttonIndex: 0 (thumb) through 4 (pinky)
    private void buttonEvent(int buttonIndex) {
        if (null != currentButtonState) {
            currentButtonState = currentButtonState.nextNodes[buttonIndex];
        }
    }

    private void buttonPressed(int buttonIndex) {
        totalButtonsCurrentlyPressed++;

        buttonEvent(buttonIndex);
    }

    private void buttonReleased(int buttonIndex) {
        totalButtonsCurrentlyPressed--;

        buttonEvent(buttonIndex);

        // at present, events are triggered when the last key of a sequence is released
        if (0 == totalButtonsCurrentlyPressed) {
            if (null != currentButtonState) {
                String symbol = currentButtonState.symbol;
                if (null != symbol) {
                    Modifier modifier = currentButtonState.modifier;
                    if (null == modifier) {
                        modifier = Modifier.None;
                    }

                    eventHandler.handle(currentMode, symbol, modifier);
                } else {
                    Mode mode = currentButtonState.mode;
                    // this sets the mode for *subsequent* key events
                    if (null != mode) {
                        currentMode = mode;

                        eventHandler.handle(mode, null, currentButtonState.modifier);
                    }
                }
            }

            currentButtonState = rootStates.get(currentMode);
        }
    }

    /**
     * A handler for each new combination of keyboard mode, output symbol, and symbol modifier
     */
    public interface EventHandler {
        void handle(Mode mode, String symbol, Modifier modifier);
    }
}
