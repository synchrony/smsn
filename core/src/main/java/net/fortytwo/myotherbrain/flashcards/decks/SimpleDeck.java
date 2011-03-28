package net.fortytwo.myotherbrain.flashcards.decks;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * User: josh
 * Date: 3/28/11
 * Time: 11:27 AM
 */
public class SimpleDeck extends Deck<String, String> {
    private final Map<String, Card<String, String>> cards;
    private final Deck<String, String> thisDeck = this;

    public SimpleDeck(final String name,
                      final String label) {
        super(name, label);
        this.cards = new HashMap<String, Card<String, String>>();
    }

    public void addCard(final String cardName,
                        final String question,
                        final String answer) {
        String n = Card.findCardName(cardName);
        if (null != cards.get(n)) {
            throw new IllegalStateException("card already exists in this deck: " + cardName);
        }

        Card<String, String> card = new LocalCard(n, question, answer);
        cards.put(n, card);
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
        private final String question;
        private final String answer;

        public LocalCard(final String name,
                         final String question,
                         final String answer) {
            super(name, thisDeck);
            this.question = question;
            this.answer = answer;
        }

        @Override
        public String getQuestion() {
            return question;
        }

        @Override
        public String getAnswer() {
            return answer;
        }
    }


    public static void main(final String[] args) {
        Random r = new Random();
        long i = r.nextLong();
        System.out.println(i);
    }
}
