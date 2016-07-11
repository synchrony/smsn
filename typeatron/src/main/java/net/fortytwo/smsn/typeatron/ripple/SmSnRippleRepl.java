package net.fortytwo.smsn.typeatron.ripple;

import net.fortytwo.flow.Collector;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.RippleList;
import net.fortytwo.smsn.p2p.SideEffects;
import net.fortytwo.smsn.typeatron.ChordedKeyer;
import net.fortytwo.smsn.typeatron.TypeatronControl;
import net.fortytwo.smsn.typeatron.ripple.lib.TypeatronDictionaryMapping;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SmSnRippleRepl {

    protected static final Logger logger = Logger.getLogger(SmSnRippleRepl.class.getName());

    private final RippleSession session;
    private final TypeatronDictionaryMapping typeatronDictionary;
    private final TypeatronControl typeatron;
    private final REPLEventHandler eventHandler;
    private final ExtendedCharacters extendedCharacters = new ExtendedCharacters();

    private StringBuilder currentLineOfText;

    public SmSnRippleRepl(final RippleSession session,
                          final TypeatronControl typeatron,
                          final SideEffects environment,
                          final REPLEventHandler eventHandler) throws RippleException {
        this.session = session;
        this.typeatron = typeatron;
        this.eventHandler = eventHandler;

        typeatronDictionary = new TypeatronDictionaryMapping(
                environment, typeatron);

        newLine();
    }

    private void newLine() {
        currentLineOfText = new StringBuilder();
    }

    private String getLastSymbol() {
        int n = currentLineOfText.length();
        if (n > 0) {
            char c = currentLineOfText.charAt(n - 1);
            currentLineOfText.deleteCharAt(n - 1);
            return "" + c;
        } else {
            return null;
        }
    }

    public void handle(final String symbol,
                       final ChordedKeyer.Modifier modifier) throws RippleException {
        //logger.log(Level.INFO, "matched symbol " + symbol + " in mode " + mode + " with modifier " + modifier);

        if (ChordedKeyer.Modifier.Control == modifier) {
            //logger.log(Level.INFO, "matched a control character");
            switch (symbol) {
                case "":
                    if (currentLineOfText.length() > 0) {
                        eventHandler.beginCommand();
                        session.push(currentLineOfText.toString());
                        session.push(typeatronDictionary);
                        newLine();
                        eventHandler.finishCommand();
                    } else {
                        logger.warning("empty command in Ripple REPL");
                    }
                    break;
                case "u": {
                    UndoRedoStack<Collector<RippleList>> undoRedoStack = session.getUndoRedoStack();
                    if (!undoRedoStack.canUndo()) {
                        throw new RippleException("can't undo");
                    } else {
                        undoRedoStack.undo();
                        typeatron.sendOkMessage();
                    }
                    break;
                }
                case "r": {
                    UndoRedoStack<Collector<RippleList>> undoRedoStack = session.getUndoRedoStack();
                    if (!undoRedoStack.canRedo()) {
                        throw new RippleException("can't undo");
                    } else {
                        undoRedoStack.redo();
                        typeatron.sendOkMessage();
                    }
                /*
            } else if (symbol.equals("u")) { // "to upper case" character primitive
                String s = getLastSymbol();
                if (null != s) {
                    currentLineOfText.append(s.toUpperCase());
                }
                */
                    break;
                }
                case "n": { // "to number" character primitive
                    String s = getLastSymbol();
                    if (null != s) {
                        char c = s.charAt(0);
                        if (c == 'o') {
                            currentLineOfText.append("0");
                        } else if (c >= 'a' && c <= 'i') {
                            currentLineOfText.append((char) (s.charAt(0) - 'a' + '1'));
                        }
                    }
                    break;
                }
                case "p": { // "to punctuation" character primitive
                    String s = getLastSymbol();
                    if (null != s) {
                        String p = typeatron.getKeyer().getPunctuationMap().get(s);
                        if (null != p) {
                            currentLineOfText.append(p);
                        }
                    }
                    break;
                }
                case "'":
                    applyDiacritic(ExtendedCharacters.Diacritic.Acute);
                    break;
                case "`":
                    applyDiacritic(ExtendedCharacters.Diacritic.Grave);
                    break;
                case "^":
                    applyDiacritic(ExtendedCharacters.Diacritic.Circumflex);
                    break;
                case "\"":
                    applyDiacritic(ExtendedCharacters.Diacritic.Dieresis);
                    break;
                case "~":
                    applyDiacritic(ExtendedCharacters.Diacritic.Tilde);
                    // TODO: slash "diacritic"
                    break;
                default:
                    logger.log(Level.WARNING, "unknown control value: " + symbol);
                    //currentLineOfText.append("C-" + symbol);
                    break;
            }
        } else if (ChordedKeyer.Modifier.None == modifier) {
            if (symbol.equals("\n")) { // handle newline
                if (currentLineOfText.length() > 0) {
                    session.push(currentLineOfText.toString());
                    newLine();
                }
            } else if (symbol.equals(ChordedKeyer.SpecialChar.DEL.name())) { // handle delete
                if (currentLineOfText.length() > 0) {
                    currentLineOfText.deleteCharAt(currentLineOfText.length() - 1);
                }
            } else if (symbol.equals(ChordedKeyer.SpecialChar.ESC.name())) { // handle escape
                // TODO: nothing else?
                newLine();
            } else { // handle ordinary text
                currentLineOfText.append(symbol);
            }
        } else {
            throw new IllegalStateException("unexpected modifier: " + modifier);
        }
    }

    private void applyDiacritic(final ExtendedCharacters.Diacritic d) {
        if (0 == currentLineOfText.length()) {
            logger.warning("applied diacritic " + d + " to empty string");
            typeatron.sendWarningMessage();
        } else {
            char c = currentLineOfText.charAt(currentLineOfText.length() - 1);
            Character cm = extendedCharacters.modify(d, c);
            if (null == cm) {
                logger.warning("diacritic " + d + " cannot modify character '" + c + "'");
                typeatron.sendWarningMessage();
            } else {
                currentLineOfText.deleteCharAt(currentLineOfText.length() - 1);
                currentLineOfText.append(cm);
            }
        }
    }

    public interface REPLEventHandler {
        void beginCommand();

        void finishCommand();
    }

}
