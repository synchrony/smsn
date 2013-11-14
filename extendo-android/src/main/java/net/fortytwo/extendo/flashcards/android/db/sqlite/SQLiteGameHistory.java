package net.fortytwo.extendo.flashcards.android.db.sqlite;

import android.content.ContentValues;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import net.fortytwo.extendo.flashcards.Card;
import net.fortytwo.extendo.flashcards.Deck;
import net.fortytwo.extendo.flashcards.Trial;
import net.fortytwo.extendo.flashcards.db.CloseableIterator;
import net.fortytwo.extendo.flashcards.db.GameHistory;

import java.io.IOException;

/**
 * Note: in ordering results by increasing HISTORY__ID instead of HISTORY__TIME, it is assumed that these have the same order.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SQLiteGameHistory extends GameHistory {
    public static final String
            CARDS = "cards",
            HISTORY = "history";
    public static final String
            CARDS__ID = "_id",
            CARDS__DECK = "deck",
            CARDS__CARD = "card",
            CARDS__DATA = "data";
    public static final String
            HISTORY__ID = "_id",
            HISTORY__DECK = "deck",
            HISTORY__CARD = "card",
            HISTORY__RESULT = "result",
            HISTORY__TIME = "time";

    private final String[] COLUMNS = new String[]{HISTORY__ID, HISTORY__DECK, HISTORY__CARD, HISTORY__TIME, HISTORY__RESULT};

    private final SQLiteDatabase database;

    public SQLiteGameHistory(final SQLiteDatabase database) {
        this.database = database;
        //correctUnicodeIssue();
    }

    public void close() throws IOException {
        // Do nothing (the database is to be closed outside of this history).
    }

    public void clear() throws IOException {
        database.delete(SQLiteGameHistory.HISTORY, null, null);
    }

    /*
    private final Pattern BAD_ESCAPE = Pattern.compile("\\\\u[0-9][0-9][0-9][0-9][0-9]");

    private void correctUnicodeIssue() {
        Collection<Trial> allTrials = new LinkedList<Trial>();
        CloseableIterator<Trial> iter = getHistory();
        while (iter.hasNext()) {
            allTrials.add(iter.next());
        }
        iter.close();

        for (Trial t : allTrials) {
            String cardName = t.getCardName();
            String newName = replaceBadEscapes(cardName);
            if (!cardName.equals(newName)) {
                System.out.println("replacing " + cardName + " with " + newName);
                ContentValues up = new ContentValues();
                up.put(HISTORY__CARD, newName);
                db.update(HISTORY, up, "deck=? AND card=? AND time=?", new String[]{t.getDeckName(), t.getCardName(), String.valueOf(t.getTime())});
            }
        }
    }

    private String replaceBadEscapes(String s) {
        Matcher m = BAD_ESCAPE.matcher(s);
        if (m.find()) {
            String t = s.substring(0, m.start())
                    + unicode(Integer.valueOf(s.substring(m.start() + 2, m.end())))
                    + s.substring(m.end());
            return replaceBadEscapes(t);
        } else {
            return s;
        }
    }

    private String unicode(final int c) {
        StringBuilder sb = new StringBuilder("\\u");
        sb.append(VocabularyDeck.HEX_CHARS[(c >> 12) & 0xF]);
        sb.append(VocabularyDeck.HEX_CHARS[(c >> 8) & 0xF]);
        sb.append(VocabularyDeck.HEX_CHARS[(c >> 4) & 0xF]);
        sb.append(VocabularyDeck.HEX_CHARS[c & 0xF]);
        return sb.toString();
    }  */

    @Override
    public void log(final Trial trial) {
        ContentValues cv = new ContentValues();
        cv.put(HISTORY__DECK, trial.getDeckName());
        cv.put(HISTORY__CARD, trial.getCardName());
        cv.put(HISTORY__TIME, String.valueOf(trial.getTime()));
        //cv.put(HISTORY__TIME, trial.getTime());
        cv.put(HISTORY__RESULT, trial.getResult().toString());
        database.insert(HISTORY, HISTORY__ID, cv);
    }

    @Override
    public CloseableIterator<Trial> getHistory() {
        return new CursorIterator(database.query(HISTORY,
                COLUMNS,
                null,
                null,
                null,
                null,
                HISTORY__ID));
    }

    @Override
    public CloseableIterator<Trial> getHistory(final Deck deck) {
        return new CursorIterator(database.query(HISTORY,
                COLUMNS,
                HISTORY__DECK + "=?",
                new String[]{deck.getName()},
                null,
                null,
                HISTORY__ID));
    }

    @Override
    public CloseableIterator<Trial> getHistory(final Card card) {
        return new CursorIterator(database.query(HISTORY,
                COLUMNS,
                HISTORY__DECK + "=? AND " + HISTORY__CARD + "=?",
                new String[]{card.getDeck().getName(), card.getName()},
                null,
                null,
                HISTORY__ID));
    }

    private class CursorIterator implements CloseableIterator<Trial> {
        private final Cursor cursor;

        public CursorIterator(final Cursor cursor) {
            this.cursor = cursor;
            cursor.moveToFirst();
        }

        public void close() {
            cursor.close();
        }

        public boolean hasNext() {
            return !cursor.isAfterLast();
        }

        public Trial next() {
            int id = cursor.getInt(0);
            String deckName = cursor.getString(1);
            String cardName = cursor.getString(2);
            //System.out.println(cardName);
            long time = Long.valueOf(cursor.getString(3));
            //long time = (long) cursor.getInt(3);
            Trial.Result result = Trial.Result.valueOf(cursor.getString(4));
            Trial t = new Trial(deckName, cardName, time, result);

            cursor.moveToNext();
            return t;
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    private abstract class CompoundCursorIterator implements CloseableIterator<Trial> {
        private CloseableIterator<Trial> cur;
        private int pageNumber = 0;
        private final int pageSize = 500;
        private int count = 0;

        public void close() {
            if (null != cur) {
                cur.close();
            }
        }

        public boolean hasNext() {
            if (null == cur || count == pageSize) {
                if (null != cur) {
                    cur.close();
                }
                cur = new CursorIterator(createCursor(pageNumber, pageSize));
                count = 0;
            }

            return cur.hasNext();
        }

        public Trial next() {
            count++;
            return cur.next();
        }

        public void remove() {
            cur.remove();
        }

        public abstract Cursor createCursor(int pageNumber,
                                            int pageSize);
    }
}
