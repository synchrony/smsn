package net.fortytwo.myotherbrain.flashcards.db;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;
import net.fortytwo.myotherbrain.flashcards.Trial;

import java.io.IOException;

/**
 * User: josh
 * Date: 3/14/11
 * Time: 6:46 PM
 */
public abstract class GameHistory {
    public abstract void log(Trial trial) throws IOException;

    public abstract CloseableIterator<Trial> getHistory();

    public abstract CloseableIterator<Trial> getHistory(Deck deck);

    public abstract CloseableIterator<Trial> getHistory(Card card);

    public abstract void clear() throws IOException;

    public abstract void close() throws IOException;
}
