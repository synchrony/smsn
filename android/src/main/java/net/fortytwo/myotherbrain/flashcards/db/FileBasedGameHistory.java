package net.fortytwo.myotherbrain.flashcards.db;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;
import net.fortytwo.myotherbrain.flashcards.Trial;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * User: josh
 * Date: 3/14/11
 * Time: 8:49 PM
 */
public class FileBasedGameHistory extends GameHistory {
    private final List<Trial> trials;
    private final BufferedWriter writer;

    public FileBasedGameHistory(final File db) throws IOException {
        trials = new LinkedList<Trial>();

        if (db.exists()) {
            InputStream is = new FileInputStream(db);
            try {
                BufferedReader br = new BufferedReader(new InputStreamReader(is));
                String line;
                while (null != (line = br.readLine())) {
                    String[] args = line.split("\t");
                    Trial t = new Trial(args[0], args[1], Long.valueOf(args[2]), Trial.Result.valueOf(args[3]));
                    trials.add(t);
                    //System.out.println(line);
                }
            } finally {
                is.close();
            }
        }

        //OutputStream os = new FileOutputStream(db);
        //writer = new PrintWriter(os, true);

        // Open the file for append.  Note: the writer is never closed.
        writer = new BufferedWriter(new FileWriter(db, true));
    }

    public void log(final Trial trial) throws IOException {
        //trials.add(0, trial);
        trials.add(trial);
        writer.write(trial.tabDelimited());
        writer.write('\n');
        writer.flush();
    }

    public CloseableIterator<Trial> getHistory(final Deck deck) {
        List<Trial> h = new LinkedList<Trial>();
        for (Trial t : trials) {
            if (t.getDeckName().equals(deck.getName())) {
                h.add(t);
            }
        }

        return new TrivialCloseableIterator<Trial>(h.iterator());
    }

    public CloseableIterator<Trial> getHistory(final Deck deck,
                                               final Card card) {
        List<Trial> h = new LinkedList<Trial>();
        for (Trial t : trials) {
            if (t.getDeckName().equals(deck.getName())
                    && t.getCardName().equals(card.getName())) {
                h.add(t);
            }
        }

        return new TrivialCloseableIterator<Trial>(h.iterator());
    }

    private class TrivialCloseableIterator<T> implements CloseableIterator<T> {
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
}
