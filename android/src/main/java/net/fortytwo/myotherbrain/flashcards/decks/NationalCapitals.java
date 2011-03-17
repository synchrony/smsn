package net.fortytwo.myotherbrain.flashcards.decks;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * User: josh
 * Date: 3/14/11
 * Time: 11:50 AM
 */
// TODO: use country codes as keys, instead of full names
public class NationalCapitals extends Deck<String, String> {
    private final Map<String, String> answers;
    private final Map<String, Card<String, String>> cards = new HashMap<String, Card<String, String>>();

    public NationalCapitals() throws IOException {
        super("national_capitals");

        answers = new HashMap<String, String>();

        // List of national capital cities retrieved on 2011-03-14 from:
        //     http://geography.about.com/od/countryinformation/a/capitals.htm
        InputStream is = NationalCapitals.class.getResourceAsStream("national_capitals.txt");
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(is));
            String l;
            while ((l = br.readLine()) != null) {
                l = l.trim();
                if (0 < l.length()) {
                    int i = l.indexOf(":");
                    String q = l.substring(0, i).trim();
                    String a = l.substring(i + 1).trim();
                    answers.put(q, a);
                }
            }
        } finally {
            is.close();
        }

        for (String s : answers.keySet()) {
            Card<String, String> c = new LocalCard(s, this);
            cards.put(c.getName(), c);
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
        public LocalCard(final String name,
                         final Deck deck) {
            super(name, deck);
        }

        @Override
        public String getQuestion() {
            return "What is the capital city of " + getName() + "?";
        }

        @Override
        public String getAnswer() {
            return answers.get(getName());
        }
    }
}
