package net.fortytwo.myotherbrain.flashcards.decks.vocab;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;
import net.fortytwo.myotherbrain.flashcards.db.CardStore;
import net.fortytwo.myotherbrain.flashcards.db.CloseableIterator;

import java.io.IOException;
import java.util.List;
import java.util.Locale;

/**
 * A deck of vocabulary terms with pronunciation and meaning.
 * <p/>
 * User: josh
 * Date: 3/9/11
 * Time: 6:04 PM
 */
public abstract class VocabularyDeck extends Deck<String, String> {
    public enum Format {TEXT, HTML}

    protected final Locale locale;
    private final CardStore<String, String> store;

    public VocabularyDeck(final String name,
                          final String label,
                          final Locale locale,
                          final Format format,
                          final CardStore<String, String> store) throws IOException {
        super(name, label);

        this.store = store;
        this.locale = locale;

        Dictionary d = createVocabulary();
        for (String key : d.getKeys()) {
            List<Term> defs = d.getDefinitions(key);

            String n = Card.findCardName(key);
            store.add(new VocabularyCard(n, this, defs, format));
        }
    }

    protected abstract Dictionary createVocabulary() throws IOException;

    @Override
    public CloseableIterator<Card<String, String>> getCards() {
        return store.findAll(this);
    }

    public Card<String, String> getCard(final String name) {
        return store.find(this, name);
    }

    public static String htmlEscape(final String s) {
        StringBuffer sb = new StringBuffer(s.length());
        // true if last char was blank
        boolean lastWasBlankChar = false;
        int len = s.length();
        char c;

        for (int i = 0; i < len; i++) {
            c = s.charAt(i);
            if (c == ' ') {
                // blank gets extra work,
                // this solves the problem you get if you replace all
                // blanks with &nbsp;, if you do that you loss
                // word breaking
                if (lastWasBlankChar) {
                    lastWasBlankChar = false;
                    sb.append("&nbsp;");
                } else {
                    lastWasBlankChar = true;
                    sb.append(' ');
                }
            } else {
                lastWasBlankChar = false;
                //
                // HTML Special Chars
                if (c == '"')
                    sb.append("&quot;");
                else if (c == '&')
                    sb.append("&amp;");
                else if (c == '<')
                    sb.append("&lt;");
                else if (c == '>')
                    sb.append("&gt;");
                else if (c == '\n')
                    // Handle Newline
                    sb.append("<br/>");
                else {
                    int ci = 0xffff & c;
                    if (ci < 160)
                        // nothing special only 7 Bit
                        sb.append(c);
                    else {
                        // Not 7 Bit use the unicode system
                        sb.append("&#");
                        sb.append(Integer.toString(ci));
                        sb.append(';');
                    }
                }
            }
        }

        //System.out.println("converted '" + s + "' to '" + sb.toString() + "'");
        return sb.toString();
    }
}
