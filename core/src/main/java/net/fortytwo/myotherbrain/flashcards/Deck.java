package net.fortytwo.myotherbrain.flashcards;

import net.fortytwo.myotherbrain.flashcards.db.CloseableIterator;

/**
 * A set of cards with a common theme.
 * <p/>
 * User: josh
 * Date: 3/5/11
 * Time: 6:59 PM
 */
public abstract class Deck<Q, A> {
    private final String name;
    private final String label;

    public Deck(final String name,
                final String label) {
        this.name = name;
        this.label = label;
    }

    public String getName() {
        return name;
    }

    public String getLabel() {
        return label;
    }

    public abstract Card<Q, A> getCard(String name);

    public abstract CloseableIterator<Card<Q, A>> getCards();
}
