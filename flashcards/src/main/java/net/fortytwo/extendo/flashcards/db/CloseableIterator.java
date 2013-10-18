package net.fortytwo.extendo.flashcards.db;

import java.util.Iterator;

/**
 * User: josh
 * Date: 3/14/11
 * Time: 6:45 PM
 */
public interface CloseableIterator<T> extends Iterator<T> {
    void close();
}
