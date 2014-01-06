package net.fortytwo.extendo.brainstem;

import android.widget.EditText;
import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.Main;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypeatronControl extends BluetoothDeviceControl {
    private final Main.Toaster toaster;

    private byte[] lastInput = "00000".getBytes();

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


    private class StateNode {
        public String symbol;
        public StateNode[] nextNodes = new StateNode[5];
    }

    private StateNode newStateNode() {

        return new StateNode();
    }

    private StateNode stateTree;
    private StateNode currentButtonState;
    private int totalButtonsCurrentlyPressed;

    private void addKeyMapping(String sequence, String symbol) {
        StateNode cur = stateTree;
        int l = sequence.length();
        for (int j = 0; j < l; j++) {
            int index = sequence.charAt(j) - 49;
            StateNode next = cur.nextNodes[index];
            if (null == next) {
                next = newStateNode();
                cur.nextNodes[index] = next;
            }

            cur = next;
        }

        if (null != cur.symbol) {
            System.err.println("conflicting symbols for sequence " + sequence);
        } else {
            cur.symbol = symbol;
        }
    }

    private void setupParser() {
        // TODO: we shouldn't assume the device powers up with no buttons pressed, although this is likely
        totalButtonsCurrentlyPressed = 0;

        stateTree = newStateNode();
        currentButtonState = stateTree;

        // map keys in individual calls, rather than wasting valuable SRAM on a temporary array
        addKeyMapping("2112", "a");
        //addKeyMapping("2121", ""},
        addKeyMapping("2332", "e");
        addKeyMapping("2323", "w");
        addKeyMapping("2442", "i");
        addKeyMapping("2424", "y");
        addKeyMapping("2552", "o");
        addKeyMapping("2525", "u");
        addKeyMapping("3113", "p");
        addKeyMapping("3131", "b");
        addKeyMapping("3223", "t");
        addKeyMapping("3232", "d");
        addKeyMapping("3443", "k");
        addKeyMapping("3434", "g");
        addKeyMapping("3553", "q");
        //addKeyMapping("3535", ""},
        addKeyMapping("4114", "f");
        addKeyMapping("4141", "v");
        addKeyMapping("4224", "c");
        addKeyMapping("4242", "j");
        addKeyMapping("4334", "s");
        addKeyMapping("4343", "z");
        addKeyMapping("4554", "h");
        addKeyMapping("4545", "x");
        addKeyMapping("5115", "m");
        //addKeyMapping("5151", ""},
        addKeyMapping("5225", "n");
        //addKeyMapping("5252", ""},
        addKeyMapping("5335", "l");
        //addKeyMapping("5353", ""},
        addKeyMapping("5445", "r");
        //addKeyMapping("5454", ""}
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
                }
            }

            currentButtonState = stateTree;
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
