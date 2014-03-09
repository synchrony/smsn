package net.fortytwo.extendo.flashcards.db;

import net.fortytwo.extendo.flashcards.Card;
import net.fortytwo.extendo.flashcards.Deck;
import net.fortytwo.extendo.flashcards.Trial;

import java.io.IOException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class GameHistory {
    public abstract void log(Trial trial) throws IOException;

    public abstract CloseableIterator<Trial> getHistory();

    public abstract CloseableIterator<Trial> getHistory(Deck deck);

    public abstract CloseableIterator<Trial> getHistory(Card card);

    public abstract void clear() throws IOException;

    public abstract void close() throws IOException;
}
