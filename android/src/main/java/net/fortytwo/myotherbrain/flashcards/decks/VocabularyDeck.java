package net.fortytwo.myotherbrain.flashcards.decks;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * A deck of Chinese characters with pronunciation and meaning.
 * <p/>
 * User: josh
 * Date: 3/9/11
 * Time: 6:04 PM
 */
public abstract class VocabularyDeck extends Deck<String, String> {
    private final Map<String, Term> terms;
    private final Map<String, Card<String, String>> cards = new HashMap<String, Card<String, String>>();

    protected class Term {
        public String type;
        public String normativeForm;
        public String alternativeForm;
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
            if (null != term.alternativeForm) {
                sb.append(" (").append(term.alternativeForm).append(")");
            }
            sb.append(" ").append(term.pronunciation)
                    .append(" -- ");
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
