package net.fortytwo.extendo.flashcards.db;

import net.fortytwo.extendo.flashcards.Card;
import net.fortytwo.extendo.flashcards.Deck;

import java.io.IOException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface CardSerializer<Q, A> {
    String serialize(Card<Q, A> card) throws IOException;

    Card<Q, A> deserialize(String name,
                           Deck<Q, A> deck,
                           String data) throws IOException;
}
