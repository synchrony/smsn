package net.fortytwo.extendo.brainstem;

import android.widget.EditText;
import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.Main;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypeatronControl extends BluetoothDeviceControl {
    private final Main.Toaster toaster;

    private byte[] lastInput = "00000".getBytes();

    private enum Mode {LowercaseText, UppercaseText, Control, Punctuation, Numeric, Mash}

    private class StateNode {
        /**
         * A symbol emitted when this state is reached
         */
        public String symbol;

        /**
         * A mode entered when this state is reached
         */
        public Mode mode;

        public StateNode[] nextNodes = new StateNode[5];
    }

    private Map<Mode, StateNode> rootStates;

    private Mode currentMode;
    private StateNode currentButtonState;
    private int totalButtonsCurrentlyPressed;

    public TypeatronControl(final String address,
                            final OSCDispatcher oscDispatcher,
                            final EditText textEditor,
                            final Main.Toaster toaster) {
        super(address);
        this.toaster = toaster;

        setupParser();

        oscDispatcher.register("/exo/tt/keys", new OSCMessageHandler() {
            public void handle(final OSCMessage message) {
                Object[] args = message.getArguments();
                if (1 == args.length) {
                    inputReceived(((String) args[0]).getBytes());
                    textEditor.setText("Typeatron keys: " + args[0] + " (" + totalButtonsCurrentlyPressed + " pressed)");
                } else {
                    textEditor.setText("Typeatron control error (wrong # of args)");
                }
            }
        });

        oscDispatcher.register("/exo/tt/error", new OSCMessageHandler() {
            public void handle(OSCMessage message) {
                Object[] args = message.getArguments();
                if (1 == args.length) {
                    textEditor.append("\nTypeatron device error: " + args[0]);
                } else {
                    textEditor.setText("Typeatron control error (wrong # of args)");
                }
            }
        });
    }

    private void addChord(final String sequence,
                          final Mode outputMode,
                          final String outputSymbol,
                          final Mode inputMode) {
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
                throw new IllegalStateException("conflicting symbols for sequence " + sequence);
            } else {
                cur.symbol = outputSymbol;
            }
        } else if (null != outputMode) {
            if (null != cur.mode && cur.mode != outputMode) {
                throw new IllegalArgumentException("conflicting output modes for sequence " + sequence);
            } else {
                cur.mode = outputMode;
            }
        } else {
            throw new IllegalArgumentException("either output mode or symbol must be non-null");
        }

        if (null != cur.mode && null != cur.symbol) {
            throw new IllegalStateException("sequence has been assigned both an output symbol and an output mode: " + sequence);
        }
    }

    private void setupParser() {
        // TODO: we shouldn't assume the device powers up with no buttons pressed, although this is likely
        totalButtonsCurrentlyPressed = 0;

        rootStates = new HashMap<Mode, StateNode>();
        for (Mode m : Mode.values()) {
            rootStates.put(m, new StateNode());
        }

        currentMode = Mode.LowercaseText;
        currentButtonState = rootStates.get(currentMode);

        // how get back to default mode from anywhere other than mash mode
        for (Mode m : Mode.values()) {
            if (m != Mode.Mash) {
                addChord("11", Mode.LowercaseText, null, m);
            }
        }

        // mode entry from default mode
        addChord("1221", Mode.Control, null, Mode.LowercaseText);
        // 1212 unassigned
        addChord("1331", Mode.Punctuation, null, Mode.LowercaseText);
        addChord("1313", Mode.Numeric, null, Mode.LowercaseText);
        addChord("1441", Mode.UppercaseText, null, Mode.LowercaseText);
        addChord("1414", Mode.LowercaseText, null, Mode.LowercaseText);  // a no-op
        addChord("1551", Mode.Mash, null, Mode.LowercaseText);
        // 1515 unassigned

        // break out of mash mode
        addChord("1234554321", Mode.LowercaseText, null, Mode.Mash);

        // space, newline, delete, escape available in all of the text-entry modes
        for (Mode m : new Mode[]{Mode.LowercaseText, Mode.UppercaseText, Mode.Punctuation, Mode.Numeric}) {
            addChord("22", null, "SPACE", m);
            addChord("33", null, "RET", m);
            addChord("44", null, "DEL", m);
            addChord("55", null, "ESC", m);
        }

        addChord("2112", null, "a", Mode.LowercaseText);
        addChord("2112", null, "A", Mode.UppercaseText);
        addChord("2112", null, "'", Mode.Punctuation);

        // 2121 unassigned

        addChord("2332", null, "e", Mode.LowercaseText);
        addChord("2332", null, "E", Mode.UppercaseText);
        addChord("2332", null, "=", Mode.Punctuation);

        addChord("2323", null, "w", Mode.LowercaseText);
        addChord("2323", null, "W", Mode.UppercaseText);
        addChord("2323", null, "@", Mode.Punctuation);

        addChord("2442", null, "i", Mode.LowercaseText);
        addChord("2442", null, "I", Mode.UppercaseText);
        addChord("2442", null, ":", Mode.Punctuation);

        addChord("2424", null, "y", Mode.LowercaseText);
        addChord("2424", null, "Y", Mode.UppercaseText);
        addChord("2424", null, "&", Mode.Punctuation);

        addChord("2552", null, "o", Mode.LowercaseText);
        addChord("2552", null, "O", Mode.UppercaseText);
        // no punctuation associated with "o"

        addChord("2525", null, "u", Mode.LowercaseText);
        addChord("2525", null, "U", Mode.UppercaseText);
        addChord("2525", null, "_", Mode.Punctuation);

        addChord("3113", null, "p", Mode.LowercaseText);
        addChord("3113", null, "P", Mode.UppercaseText);
        addChord("3113", null, "+", Mode.Punctuation);

        addChord("3131", null, "b", Mode.LowercaseText);
        addChord("3131", null, "B", Mode.UppercaseText);
        addChord("3131", null, "\\", Mode.Punctuation);

        addChord("3223", null, "t", Mode.LowercaseText);
        addChord("3223", null, "T", Mode.UppercaseText);
        addChord("3223", null, "~", Mode.Punctuation);

        addChord("3232", null, "d", Mode.LowercaseText);
        addChord("3232", null, "D", Mode.UppercaseText);
        addChord("3232", null, "$", Mode.Punctuation);

        addChord("3443", null, "k", Mode.LowercaseText);
        addChord("3443", null, "K", Mode.UppercaseText);
        addChord("3443", null, "*", Mode.Punctuation);

        addChord("3434", null, "g", Mode.LowercaseText);
        addChord("3434", null, "G", Mode.UppercaseText);
        addChord("3434", null, "`", Mode.Punctuation);

        addChord("3553", null, "q", Mode.LowercaseText);
        addChord("3553", null, "Q", Mode.UppercaseText);
        addChord("3553", null, "?", Mode.Punctuation);

        // 3535 unassigned

        addChord("4114", null, "f", Mode.LowercaseText);
        addChord("4114", null, "F", Mode.UppercaseText);
        addChord("4114", null, ".", Mode.Punctuation);

        addChord("4141", null, "v", Mode.LowercaseText);
        addChord("4141", null, "V", Mode.UppercaseText);
        addChord("4141", null, "|", Mode.Punctuation);

        addChord("4224", null, "c", Mode.LowercaseText);
        addChord("4224", null, "C", Mode.UppercaseText);
        addChord("4224", null, ",", Mode.Punctuation);

        addChord("4242", null, "j", Mode.LowercaseText);
        addChord("4242", null, "J", Mode.UppercaseText);
        addChord("4242", null, ";", Mode.Punctuation);

        addChord("4334", null, "s", Mode.LowercaseText);
        addChord("4334", null, "S", Mode.UppercaseText);
        addChord("4334", null, "/", Mode.Punctuation);

        addChord("4343", null, "z", Mode.LowercaseText);
        addChord("4343", null, "Z", Mode.UppercaseText);
        addChord("4343", null, "%", Mode.Punctuation);

        addChord("4554", null, "h", Mode.LowercaseText);
        addChord("4554", null, "H", Mode.UppercaseText);
        addChord("4554", null, "^", Mode.Punctuation);

        addChord("4545", null, "x", Mode.LowercaseText);
        addChord("4545", null, "X", Mode.UppercaseText);
        addChord("4545", null, "!", Mode.Punctuation);

        addChord("5115", null, "m", Mode.LowercaseText);
        addChord("5115", null, "M", Mode.UppercaseText);
        addChord("5115", null, "-", Mode.Punctuation);

        // 5151 unassigned

        addChord("5225", null, "n", Mode.LowercaseText);
        addChord("5225", null, "N", Mode.UppercaseText);
        addChord("5225", null, "#", Mode.Punctuation);

        // 5252 unassigned

        addChord("5335", null, "l", Mode.LowercaseText);
        addChord("5335", null, "L", Mode.UppercaseText);
        addChord("5335", null, "\"", Mode.Punctuation);

        // 5353 unassigned

        addChord("5445", null, "r", Mode.LowercaseText);
        addChord("5445", null, "R", Mode.UppercaseText);
        // no punctuation associated with "r" (reserved for right-handed quotes)

        // 5454 unassigned
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
                    toaster.makeText("typed: " + symbol);
                } else {
                    Mode mode = currentButtonState.mode;
                    if (null != mode) {
                        currentMode = mode;
                        toaster.makeText("entered mode: " + mode);
                    }
                }
            }

            currentButtonState = rootStates.get(currentMode);
        }
    }

    private void inputReceived(byte[] input) {
        for (int i = 0; i < 5; i++) {
            // Generally, at most one button should change per time step
            // However, if two buttons change state, it is an arbitrary choice w.r.t. which one changed first
            if (input[i] != lastInput[i]) {
                if ('1' == input[i]) {
                    buttonPressed(i);
                } else {
                    buttonReleased(i);
                }
            }
        }

        lastInput = input;
    }
}
