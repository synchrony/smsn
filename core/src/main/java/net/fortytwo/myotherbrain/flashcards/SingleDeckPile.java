package net.fortytwo.myotherbrain.flashcards;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 * User: josh
 * Date: 3/17/11
 * Time: 5:07 PM
 */
public class SingleDeckPile<Q, A> implements Pile<Q, A> {
    protected final Deck<Q, A> deck;
    protected final List<Card> pool;

    public SingleDeckPile(final Deck<Q, A> deck) {
        this.deck = deck;

        // Create the pool of all cards.
        pool = new LinkedList<Card>();
        pool.addAll(deck.getCards());

        // Randomize the pool, so that every cold start is not the same.
        Collections.shuffle(pool);
    }

    public Card drawCard(final String deckName,
                         final String cardName) {
        if (!deckName.equals(deck.getName())) {
            return null;
        }

        Card c = deck.getCard(cardName);
        if (null != c) {
            pool.remove(c);
            return c;
        } else {
            return null;
        }
    }

    public Card drawRandomCard() {
        return pool.remove(0);
    }

    public boolean isEmpty() {
        return 0 == pool.size();
    }
}
