package net.fortytwo.myotherbrain.flashcards;

import java.util.Random;

/**
 * User: josh
 * Date: 3/5/11
 * Time: 7:01 PM
 */
public abstract class Card<Q, A> {
    private static final long
            FIRST_DELAY_CORRECT = 60000,
            FIRST_DELAY_INCORRECT = 30000;

    // Randomized delays will be within this ratio of the precise value.
    private static final double IMPRECISION = 0.1;

    private final Random random = new Random();

    private final String name;
    private final Deck deck;

    private long lastTrial = 0;
    private long nextTrial = 0;

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

    public void correct(final long now) {
        long delay = 0 == lastTrial
                ? FIRST_DELAY_CORRECT
                : increaseDelay(now - lastTrial);
        delay = randomizeDelay(delay);
        nextTrial = now + delay;
        lastTrial = now;
    }

    public void incorrect(final long now) {
        lastTrial = now;
        long delay = randomizeDelay(FIRST_DELAY_INCORRECT);
        nextTrial = lastTrial + delay;
    }

    public long getNextTrial() {
        return nextTrial;
    }

    private long increaseDelay(final long delay) {
        return delay * 2;
    }

    private long randomizeDelay(final long delay) {
        long d = (long) (IMPRECISION * delay * (random.nextDouble() * 2 - 1));
        return delay + d;
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
