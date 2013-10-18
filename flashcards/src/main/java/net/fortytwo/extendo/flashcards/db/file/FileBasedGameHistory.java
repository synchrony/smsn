package net.fortytwo.extendo.flashcards.db.file;

import net.fortytwo.extendo.flashcards.Card;
import net.fortytwo.extendo.flashcards.Deck;
import net.fortytwo.extendo.flashcards.Trial;
import net.fortytwo.extendo.flashcards.db.CloseableIterator;
import net.fortytwo.extendo.flashcards.db.GameHistory;
import net.fortytwo.extendo.flashcards.db.TrivialCloseableIterator;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.List;

/**
 * User: josh
 * Date: 3/14/11
 * Time: 8:49 PM
 */
public class FileBasedGameHistory extends GameHistory {
    private final List<Trial> trials;
    private FileWriter fwriter;
    private BufferedWriter writer;
    private final File db;

    public FileBasedGameHistory(final File db) throws IOException {
        this.db = db;
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

        open();
    }

    private void open() throws IOException {
        // Open the file for append.
        fwriter = new FileWriter(db, true);
        writer = new BufferedWriter(fwriter);
    }

    public void close() throws IOException {
        fwriter.close();
    }

    public void clear() throws IOException {
        close();
        new FileWriter(db).close();
        open();
    }

    public void log(final Trial trial) throws IOException {
        //trials.add(0, trial);
        trials.add(trial);
        writer.write(trial.printTabDelimited());
        writer.write('\n');
        writer.flush();
    }

    public CloseableIterator<Trial> getHistory() {
        List<Trial> h = trials;
        return new TrivialCloseableIterator<Trial>(h.iterator());
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

    public CloseableIterator<Trial> getHistory(final Card card) {
        List<Trial> h = new LinkedList<Trial>();
        for (Trial t : trials) {
            if (t.getDeckName().equals(card.getDeck().getName())
                    && t.getCardName().equals(card.getName())) {
                h.add(t);
            }
        }

        return new TrivialCloseableIterator<Trial>(h.iterator());
    }

}
