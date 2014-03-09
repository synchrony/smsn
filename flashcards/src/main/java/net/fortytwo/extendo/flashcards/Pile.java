package net.fortytwo.extendo.flashcards;

import java.util.Collection;
import java.util.List;

/**
 * A set of cards from one or more decks.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Pile<Q, A> {
    /**
     * Search for a card in the pile and, if it is present, remove it.
     * A card may be drawn any number of times, although only the first draw will affect the state of the pile.
     *
     * @param deckName the name of the deck
     * @param cardName the name of the card
     * @return the matching card
     */
    Card<Q, A> drawCard(String deckName,
                  String cardName);

    Card<Q, A> drawRandomCard();

    boolean isEmpty();
}
