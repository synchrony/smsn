package net.fortytwo.myotherbrain.flashcards.decks.geo;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;
import net.fortytwo.myotherbrain.flashcards.decks.Answer;
import net.fortytwo.myotherbrain.flashcards.decks.AnswerFormatter;
import net.fortytwo.myotherbrain.flashcards.db.CloseableIterator;
import net.fortytwo.myotherbrain.flashcards.db.TrivialCloseableIterator;
import net.fortytwo.myotherbrain.flashcards.decks.QuestionFormatter;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * User: josh
 * Date: 3/14/11
 * Time: 11:50 AM
 */
public class InternationalBorders extends Deck<String, String> {
    private final Map<String, Card<String, String>> cards = new HashMap<String, Card<String, String>>();
    private final Random random = new Random();
    private final Format format;

    public InternationalBorders(final Format format) throws IOException {
        super("international_borders", "international borders");
        this.format = format;

        for (Countries.Country c : Countries.getInstance().getCountries()) {
            if (c.neighbors.size() > 0) {
                Card<String, String> card = new LocalCard(c, this);
                cards.put(card.getName(), card);
            }
        }
    }

    @Override
    public Card<String, String> getCard(final String name) {
        return cards.get(name);
    }

    @Override
    public CloseableIterator<Card<String, String>> getCards() {
        return new TrivialCloseableIterator<Card<String, String>>(cards.values().iterator());
    }

    private class LocalCard extends Card<String, String> {
        private final Countries.Country country;
        private Countries.Country answer;

        public LocalCard(final Countries.Country country,
                         final Deck deck) {
            super(country.code, deck);
            this.country = country;
        }

        @Override
        public String getQuestion() {
            int r = random.nextInt(country.neighbors.size());
            answer = country.neighbors.get(r);

            StringBuilder sb = new StringBuilder(country.name + " borders on ");

            boolean first = true;
            for (int i = 0; i < country.neighbors.size(); i++) {
                if (i != r) {
                    if (first) {
                        first = false;
                    } else {
                        sb.append(", ");
                    }

                    sb.append(country.neighbors.get(i).name);
                }
            }

            if (first) {
                sb.append("___?");
            } else {
                sb.append(" and ___?");
            }

            QuestionFormatter t = new QuestionFormatter(deck, format);
            t.setQuestion(sb.toString());

            return t.format();
        }

        @Override
        public String getAnswer() {
            AnswerFormatter t = new AnswerFormatter(format);
            Answer a = new Answer();
            a.setSource(Countries.getInstance().getSource());
            a.setMeaning(answer.name);
            t.addAnswer(a);
            return t.format();
        }

        @Override
        public String toString() {
            return "intbord:" + country.code;
        }
    }
}
