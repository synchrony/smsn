package net.fortytwo.myotherbrain.flashcards.decks.vocab;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A deck of Chinese characters with pronunciation and meaning.
 * <p/>
 * User: josh
 * Date: 3/9/11
 * Time: 6:04 PM
 */
public abstract class VocabularyDeck extends Deck<String, String> {

    private static final char[] HEX_CHARS = {
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
    };

    private final Map<String, Card<String, String>> cards = new HashMap<String, Card<String, String>>();

    public VocabularyDeck(final String name,
                          final String label) throws IOException {
        super(name, label);

        Dictionary d = createVocabulary();
        for (String key : d.getKeys()) {
            List<Term> defs = d.getDefinitions(key);

            String n = findCardName(key);
            cards.put(n, new LocalCard(n, this, defs));
        }
    }

    public abstract Dictionary createVocabulary() throws IOException;

    @Override
    public Collection<Card<String, String>> getCards() {
        return cards.values();
    }

    public Card<String, String> getCard(final String name) {
        return cards.get(name);
    }

    protected String findCardName(final String norm) {
        return unicodeEscape(norm);
    }

    // Note: escapes both high and low (whitespace < 0x20) characters.
    private static String unicodeEscape(final String s) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c < 32 || (c >> 7) > 0) {
                sb.append("\\u");
                sb.append(HEX_CHARS[(c >> 12) & 0xF]);
                sb.append(HEX_CHARS[(c >> 8) & 0xF]);
                sb.append(HEX_CHARS[(c >> 4) & 0xF]);
                sb.append(HEX_CHARS[c & 0xF]);
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    private class LocalCard extends Card<String, String> {
        private final List<Term> defs;

        public LocalCard(final String name,
                         final Deck deck,
                         final List<Term> defs) {
            super(name, deck);

            this.defs = defs;
        }

        @Override
        public String getQuestion() {
            return defs.get(0).getForms().get(0) + " = ?";
        }

        @Override
        public String getAnswer() {
            StringBuilder sb = new StringBuilder();

            for (Term t : defs) {
                if (null != t.getSource()) {
                    sb.append("[").append(t.getSource().getLabel()).append("]\n");
                }
                sb.append(t.getForms().get(0));
                if (2 <= t.getForms().size()) {
                    sb.append(" (");
                    for (int i = 1; i < t.getForms().size(); i++) {
                        if (i > 1) {
                            sb.append("; ");
                        }
                        sb.append(t.getForms().get(i));
                    }
                    sb.append(")");
                }
                if (null != t.getPronunciation()) {
                    sb.append(" ").append(t.getPronunciation());
                }
                sb.append(" -- ");
                if (null != t.getType()) {
                    sb.append(t.getType()).append(": ");
                }
                sb.append(t.getMeaning());
                sb.append("\n");
            }

            return sb.toString();
        }

        @Override
        public String toString() {
            return defs.get(0).getForms().get(0);
        }
    }
}
