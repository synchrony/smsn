package net.fortytwo.myotherbrain.flashcards;

import java.util.Collection;

/**
 * User: josh
 * Date: 3/5/11
 * Time: 6:59 PM
 */
public abstract class Deck<Q, A> {
    private final String name;

    public Deck(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public abstract Card getCard(String name);

    public abstract Collection<Card> getCards();

    public abstract Q getQuestion(Card card);

    public abstract A getAnswer(Card card);
}
