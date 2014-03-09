package net.fortytwo.extendo.flashcards;

import net.fortytwo.extendo.flashcards.db.CloseableIterator;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SingleDeckPile<Q, A> implements Pile<Q, A> {
    private static final Logger LOGGER = Logger.getLogger(SingleDeckPile.class.getName());

    protected final Deck<Q, A> deck;
    protected final List<Card<Q, A>> pool;

    public SingleDeckPile(final Deck<Q, A> deck) {
        this.deck = deck;

        // Create the pool of all cards.
        pool = new LinkedList<Card<Q, A>>();
        CloseableIterator<Card<Q, A>> iter = deck.getCards();
        try {
            while (iter.hasNext()) {
                pool.add(iter.next());
            }
        } finally {
            iter.close();
        }

        // Randomize the pool, so that every cold start is not the same.
        Collections.shuffle(pool);
    }

    public Card<Q, A> drawCard(final String deckName,
                               final String cardName) {
        if (!deckName.equals(deck.getName())) {
            return null;
        }

        Card<Q, A> c = deck.getCard(cardName);
        if (null != c) {
            pool.remove(c);
            return c;
        } else {
            LOGGER.warning("Card not found in deck " + deckName + ": " + cardName);
            return null;
        }
    }

    public Card<Q, A> drawRandomCard() {
        return pool.remove(0);
    }

    public boolean isEmpty() {
        return 0 == pool.size();
    }
}
