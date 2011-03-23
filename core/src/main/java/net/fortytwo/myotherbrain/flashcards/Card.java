package net.fortytwo.myotherbrain.flashcards;

import java.util.Random;

/**
 * User: josh
 * Date: 3/5/11
 * Time: 7:01 PM
 */
public abstract class Card<Q, A> {

    private final String name;
    private final Deck deck;

    public long lastTrial = 0;
    public long nextTrial = 0;

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
}
