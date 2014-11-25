package net.fortytwo.extendo.typeatron.ripple;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.extendo.p2p.SideEffects;
import net.fortytwo.extendo.typeatron.ChordedKeyer;
import net.fortytwo.extendo.typeatron.TypeatronControl;
import net.fortytwo.extendo.typeatron.ripple.lib.TypeatronDictionaryMapping;
import net.fortytwo.ripple.RippleException;
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

    private StringBuilder currentLineOfText;

    public ExtendoRippleREPL(final RippleSession session,
                             final TypeatronControl typeatron,
                             final ExtendoAgent agent,
                             final SideEffects environment) throws RippleException {
        this.session = session;
        this.typeatron = typeatron;
        this.agent = agent;
        this.environment = environment;
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

    public boolean handle(final String symbol,
                       final ChordedKeyer.Modifier modifier,
                       final ChordedKeyer.Mode mode) throws RippleException {
        //logger.log(Level.INFO, "got a symbol: " + symbol + " in mode " + mode + " with modifier " + modifier);
        boolean cue = false;

        if (ChordedKeyer.Modifier.Control == modifier) {
            //logger.log(Level.INFO, "got a control character");
            if (symbol.equals("")) {
                if (currentLineOfText.length() > 0) {
                    session.push(currentLineOfText.toString());
                    session.push(typeatronDictionary);
                    newLine();
                    cue = true;
                } else {
                    logger.warning("empty text...");
                }
            } else if (symbol.equals("u")) { // "to upper case" character primitive
                String s = getLastSymbol();
                if (null != s) {
                    currentLineOfText.append(s.toUpperCase());
                }
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
            } else {
                logger.log(Level.WARNING, "unknown control value: " + symbol);
                //currentLineOfText.append("C-" + symbol);
            }
        } else if (ChordedKeyer.Modifier.None == modifier) {
            if (symbol.equals("\n")) { // handle newline
                //System.out.println("got newline");
                if (currentLineOfText.length() > 0) {
                    session.push(currentLineOfText.toString());
                    newLine();
                }
            } else if (symbol.equals("DEL")) { // handle delete
                if (currentLineOfText.length() > 0) {
                    currentLineOfText.deleteCharAt(currentLineOfText.length() - 1);
                }
            } else if (symbol.equals("ESC")) { // handle escape
                // TODO: nothing else?
                newLine();
            } else { // handle ordinary text
                System.out.println("appending: " + symbol);
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

        return cue;
    }
}
