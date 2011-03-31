package net.fortytwo.myotherbrain.flashcards.db;

import net.fortytwo.myotherbrain.flashcards.db.CloseableIterator;

import java.util.Iterator;

/**
* User: josh
* Date: 3/29/11
* Time: 8:38 PM
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
