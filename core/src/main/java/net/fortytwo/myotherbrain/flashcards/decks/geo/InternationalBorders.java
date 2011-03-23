package net.fortytwo.myotherbrain.flashcards.decks.geo;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;

import java.io.IOException;
import java.util.Collection;
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

    public InternationalBorders() throws IOException {
        super("international_borders");

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
    public Collection<Card<String, String>> getCards() {
        return cards.values();
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

            return sb.toString();
        }

        @Override
        public String getAnswer() {
            return answer.name;
        }

        @Override
        public String toString() {
            return "borders:" + country.code;
        }
    }
}
