package net.fortytwo.extendo.flashcards.db;

import net.fortytwo.extendo.flashcards.db.CloseableIterator;

import java.util.Iterator;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
*/
public class TrivialCloseableIterator<T> implements CloseableIterator<T> {
    private final Iterator<T> inner;

    public TrivialCloseableIterator(Iterator<T> inner) {
        this.inner = inner;
    }

    public void close() {
        // Do nothing.
    }

    public boolean hasNext() {
        return inner.hasNext();
    }

    public T next() {
        return inner.next();
    }

    public void remove() {
        inner.remove();
    }
}
