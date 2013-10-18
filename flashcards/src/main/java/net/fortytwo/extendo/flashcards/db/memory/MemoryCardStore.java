package net.fortytwo.extendo.flashcards.db.memory;

import net.fortytwo.extendo.flashcards.Card;
import net.fortytwo.extendo.flashcards.Deck;
import net.fortytwo.extendo.flashcards.db.CardStore;
import net.fortytwo.extendo.flashcards.db.CloseableIterator;
import net.fortytwo.extendo.flashcards.db.TrivialCloseableIterator;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * User: josh
 * Date: 3/29/11
 * Time: 4:52 PM
 */
public class MemoryCardStore<Q, A> implements CardStore<Q, A> {
    private final Map<String, Map<String, Card<Q, A>>> cards;

    public MemoryCardStore() {
        this.cards = new HashMap<String, Map<String, Card<Q, A>>>();
    }

    public void add(final Card<Q, A> card) {
        String deckName = card.getDeck().getName();
        Map<String, Card<Q, A>> d = cards.get(deckName);
        if (null == d) {
            d = new HashMap<String, Card<Q, A>>();
            cards.put(deckName, d);
        }

        d.put(card.getName(), card);
    }

    public Card<Q, A> find(final Deck<Q, A> deck,
                           final String cardName) {
        Map<String, Card<Q, A>> d = cards.get(deck.getName());
        return null == d ? null : d.get(cardName);
    }

    public CloseableIterator<Card<Q, A>> findAll(final Deck<Q, A> deck) {
        Map<String, Card<Q, A>> d = cards.get(deck.getName());
        return null == d ? null : new TrivialCloseableIterator<Card<Q, A>>(d.values().iterator());
    }

    public void clear() {
        cards.clear();
    }
}
