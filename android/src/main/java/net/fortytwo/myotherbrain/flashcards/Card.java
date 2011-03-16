package net.fortytwo.myotherbrain.flashcards;

import net.fortytwo.myotherbrain.flashcards.db.GameHistory;

/**
 * User: josh
 * Date: 3/5/11
 * Time: 7:01 PM
 */
public class Card {
    public enum Status {Active, Inactive}

    private static final long
            FIRST_DELAY_CORRECT = 60000,
            FIRST_DELAY_INCORRECT = 30000;

    private final String name;

    private long lastTrial = 0;
    private long nextTrial = 0;

    public Card(final String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void correct(final long now) {
        long delay = 0 == lastTrial
                ? FIRST_DELAY_CORRECT
                : increaseDelay(now - lastTrial);
        nextTrial = now + delay;
        lastTrial = now;
    }

    public void incorrect(final long now) {
        lastTrial = now;
        nextTrial = lastTrial + FIRST_DELAY_INCORRECT;
    }

    public long getNextTrial() {
        return nextTrial;
    }

    private long increaseDelay(final long delay) {
        return delay * 2;
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
}
