package net.fortytwo.extendo.flashcards;

import net.fortytwo.extendo.Extendo;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class Card<Q, A> {

    protected final String name;
    protected final Deck deck;

    public long lastTrial = 0;
    public long nextTrial = 0;

    /**
     * @param name a persistent, unique identifier, within the deck, for this card.
     * This identifier should not reveal the answer to the card, although it may be based on the question.
     * @param deck the deck to which this card belongs
     */
    public Card(final String name,
                final Deck deck) {
        this.name = name;
        this.deck = deck;
    }

    public abstract Q getQuestion();

    public abstract A getAnswer();

    public String getName() {
        return name;
    }

    public Deck getDeck() {
        return deck;
    }

    public long getNextTrial() {
        return nextTrial;
    }

    @Override
    public boolean equals(final Object other) {
        return other instanceof Card
                && ((Card) other).name.equals(name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public String toString() {
        return getName();
    }

    public static String findCardName(final String norm) {
        return Extendo.unicodeEscape(norm);
    }
}
