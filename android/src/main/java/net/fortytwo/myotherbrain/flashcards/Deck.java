package net.fortytwo.myotherbrain.flashcards;

import java.util.Collection;

/**
 * A set of cards with a common theme.
 *
 * User: josh
 * Date: 3/5/11
 * Time: 6:59 PM
 */
public abstract class Deck<Q, A> {
    private final String name;

    public Deck(final String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public abstract Card<Q, A> getCard(String name);

    public abstract Collection<Card<Q, A>> getCards();
}
