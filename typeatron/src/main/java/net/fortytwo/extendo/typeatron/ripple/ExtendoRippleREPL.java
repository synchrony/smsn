package net.fortytwo.extendo.typeatron.ripple;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.extendo.p2p.SideEffects;
import net.fortytwo.extendo.typeatron.ChordedKeyer;
import net.fortytwo.extendo.typeatron.TypeatronControl;
import net.fortytwo.extendo.typeatron.ripple.lib.TypeatronDictionaryMapping;
import net.fortytwo.flow.Collector;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.RippleList;
import org.openrdf.model.impl.URIImpl;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExtendoRippleREPL {

    protected static final Logger logger = Logger.getLogger(ExtendoRippleREPL.class.getName());

    private final RippleSession session;
    private final TypeatronDictionaryMapping typeatronDictionary;
    private final TypeatronControl typeatron;
    private final ExtendoAgent agent;
    private final SideEffects environment;
    private final REPLEventHandler eventHandler;
    private final ExtendedCharacters extendedCharacters = new ExtendedCharacters();

    private StringBuilder currentLineOfText;

    public ExtendoRippleREPL(final RippleSession session,
                             final TypeatronControl typeatron,
                             final ExtendoAgent agent,
                             final SideEffects environment,
                             final REPLEventHandler eventHandler) throws RippleException {
        this.session = session;
        this.typeatron = typeatron;
        this.agent = agent;
        this.environment = environment;
        this.eventHandler = eventHandler;

        UserDictionary userDictionary = new UserDictionary(typeatron);
        typeatronDictionary = new TypeatronDictionaryMapping(
                environment, typeatron, userDictionary);

        // TODO: temporary, for a demo
        String[][] shortcuts = {
                {"an", "http://fortytwo.net/2014/04/twc#AliNendick"},
                {"av", "http://fortytwo.net/2014/04/twc#AmarViswanathan"},
                {"bm", "http://fortytwo.net/2014/04/twc#BassemMakni"},
                {"bz", "http://fortytwo.net/2014/04/twc#BoliangZhang"},
                {"dy", "http://fortytwo.net/2014/04/twc#DianYu"},
                {"ea", "http://fortytwo.net/2014/04/twc#EricAmeres"},
                {"ep", "http://fortytwo.net/2014/04/twc#EvanPatton"},
                {"hw", "http://fortytwo.net/2014/04/twc#HanWang"},
                {"hl", "http://fortytwo.net/2014/04/twc#HaoLi"},
                {"hh", "http://fortytwo.net/2014/04/twc#HongzhaoHuang"},
                {"jm", "http://fortytwo.net/2014/04/twc#JamesMichaelis"},
                {"jc", "http://fortytwo.net/2014/04/twc#JimMcCusker"},
                {"jz", "http://fortytwo.net/2014/04/twc#JinGuangZheng"},
                {"je", "http://fortytwo.net/2014/04/twc#JohnErickson"},
                {"js", "http://fortytwo.net/2014/04/twc#JoshuaShinavier"},
                {"kc", "http://fortytwo.net/2014/04/twc#KatieChastain"},
                {"kg", "http://fortytwo.net/2014/04/twc#KristineGloria"},
                {"mf", "http://fortytwo.net/2014/04/twc#MattFerrito"},
                {"nr", "http://fortytwo.net/2014/04/twc#NidhiRastogi"},
                {"ql", "http://fortytwo.net/2014/04/twc#QiLi"},
                {"se", "http://fortytwo.net/2014/04/twc#SimonEllis"},
                {"tl", "http://fortytwo.net/2014/04/twc#TimLebo"},
                {"tz", "http://fortytwo.net/2014/04/twc#TongtaoZhang"},
                {"yl", "http://fortytwo.net/2014/04/twc#YueLiu"}};
        for (String[] pair : shortcuts) {
            userDictionary.put(pair[0], new URIImpl(pair[1]));
        }

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
            if (symbol.equals("")) {
                if (currentLineOfText.length() > 0) {
                    eventHandler.beginCommand();
                    session.push(currentLineOfText.toString());
                    session.push(typeatronDictionary);
                    newLine();
                    eventHandler.finishCommand();
                } else {
                    logger.warning("empty command in Ripple REPL");
                }
            } else if (symbol.equals("u")) {
                UndoRedoStack<Collector<RippleList>> undoRedoStack = session.getUndoRedoStack();
                if (!undoRedoStack.canUndo()) {
                    throw new RippleException("can't undo");
                } else {
                    undoRedoStack.undo();
                    typeatron.sendOkMessage();
                }
            } else if (symbol.equals("r")) {
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
            } else if (symbol.equals("n")) { // "to number" character primitive
                String s = getLastSymbol();
                if (null != s) {
                    char c = s.charAt(0);
                    if (c == 'o') {
                        currentLineOfText.append("0");
                    } else if (c >= 'a' && c <= 'i') {
                        currentLineOfText.append((char) (s.charAt(0) - 'a' + '1'));
                    }
                }
            } else if (symbol.equals("p")) { // "to punctuation" character primitive
                String s = getLastSymbol();
                if (null != s) {
                    String p = typeatron.getKeyer().getPunctuationMap().get(s);
                    if (null != p) {
                        currentLineOfText.append(p);
                    }
                }
            } else if (symbol.equals("'")) {
                applyDiacritic(ExtendedCharacters.Diacritic.Acute);
            } else if (symbol.equals("`")) {
                applyDiacritic(ExtendedCharacters.Diacritic.Grave);
            } else if (symbol.equals("^")) {
                applyDiacritic(ExtendedCharacters.Diacritic.Circumflex);
            } else if (symbol.equals("\"")) {
                applyDiacritic(ExtendedCharacters.Diacritic.Dieresis);
            } else if (symbol.equals("~")) {
                applyDiacritic(ExtendedCharacters.Diacritic.Tilde);
                // TODO: slash "diacritic"
            } else {
                logger.log(Level.WARNING, "unknown control value: " + symbol);
                //currentLineOfText.append("C-" + symbol);
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

        if (environment.verbose()) {
            if (agent.getFacilitatorConnection().isActive()) {
                OSCMessage m = new OSCMessage("/exo/fctr/tt/symbol");
                m.addArgument(symbol);
                agent.sendOSCMessageToFacilitator(m);
            }
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
