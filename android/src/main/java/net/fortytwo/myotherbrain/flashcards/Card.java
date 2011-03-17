package net.fortytwo.myotherbrain.flashcards;

/**
 * User: josh
 * Date: 3/5/11
 * Time: 7:01 PM
 */
public abstract class Card<Q, A> {
    private static final long
            FIRST_DELAY_CORRECT = 60000,
            FIRST_DELAY_INCORRECT = 30000;

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
