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

    private final Map<String, Term> terms;
    private final Map<String, Card<String, String>> cards = new HashMap<String, Card<String, String>>();

    protected class Term {
        public String type;
        public String normativeForm;
        public List<String> alternativeForms;
        public String pronunciation;
        public String meaning;
        public String context;
    }

    public VocabularyDeck(final String name) throws IOException {
        super(name);

        terms = createVocabulary();

        for (String s : terms.keySet()) {
            cards.put(s, new LocalCard(s, this));
        }
    }

    public abstract Map<String, Term> createVocabulary() throws IOException;

    @Override
    public Collection<Card<String, String>> getCards() {
        return cards.values();
    }

    public Card<String, String> getCard(final String name) {
        return cards.get(name);
    }

    protected String findCardName(final Term t) {
        return unicodeEscape(t.normativeForm);
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
        private final Term term;

        public LocalCard(final String name,
                         final Deck deck) {
            super(name, deck);

            term = terms.get(getName());
        }

        @Override
        public String getQuestion() {
            return term.normativeForm + " = ?";
        }

        @Override
        public String getAnswer() {
            StringBuilder sb = new StringBuilder();

            sb.append(term.normativeForm);
            if (null != term.alternativeForms && 0 < term.alternativeForms.size()) {
                sb.append(" (");
                boolean first = true;
                for (String f : term.alternativeForms) {
                    if (first) {
                        first = false;
                    } else {
                        sb.append("; ");
                    }
                    sb.append(f);
                }
                sb.append(")");
            }
            if (null != term.pronunciation) {
                sb.append(" ").append(term.pronunciation);
            }
            sb.append(" -- ");
            if (null != term.type) {
                sb.append(term.type).append(": ");
            }
            sb.append(term.meaning);

            return sb.toString();
        }

        @Override
        public String toString() {
            return term.normativeForm;
        }
    }
}
