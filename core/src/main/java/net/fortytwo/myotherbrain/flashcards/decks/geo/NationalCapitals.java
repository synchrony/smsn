package net.fortytwo.myotherbrain.flashcards.decks.geo;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;
import net.fortytwo.myotherbrain.flashcards.decks.geo.Countries;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * User: josh
 * Date: 3/14/11
 * Time: 11:50 AM
 */
public class NationalCapitals extends Deck<String, String> {
    private final Map<String, Card<String, String>> cards = new HashMap<String, Card<String, String>>();

    public NationalCapitals() throws IOException {
        super("national_capitals", "national capitals");

        for (Countries.Country c : Countries.getInstance().getCountries()) {
            Card<String, String> card = new LocalCard(c, this);
            cards.put(card.getName(), card);
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
        public LocalCard(final Countries.Country country,
                         final Deck deck) {
            super(country.code, deck);
            this.country = country;
        }

        @Override
        public String getQuestion() {
            return "What is the capital city of " + country.name + "?";
        }

        @Override
        public String getAnswer() {
            return country.capitalCity.name + "\n";
        }

        @Override
        public String toString() {
            return country.name;
        }
    }
}
