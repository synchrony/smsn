package net.fortytwo.myotherbrain.flashcards.android.db.sqlite;

import android.content.ContentValues;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;
import net.fortytwo.myotherbrain.flashcards.Trial;
import net.fortytwo.myotherbrain.flashcards.db.CloseableIterator;
import net.fortytwo.myotherbrain.flashcards.db.GameHistory;

/**
 * User: josh
 * Date: 3/14/11
 * Time: 6:48 PM
 */
public class SQLiteGameHistory extends GameHistory {
    public static final String
            HISTORY = "history";
    public static final String
            HISTORY__ID = "_id",
            HISTORY__DECK = "deck",
            HISTORY__CARD = "card",
            HISTORY__RESULT = "result",
            HISTORY__TIME = "time";

    private final SQLiteDatabase database;

    public SQLiteGameHistory(final SQLiteDatabase database) {
        this.database = database;
        //correctUnicodeIssue();
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
        String[] cols = new String[]{HISTORY__DECK, HISTORY__CARD, HISTORY__TIME, HISTORY__RESULT};
        return new CursorIterator(database.query(HISTORY,
                cols,
                null,
                null,
                null,
                null,
                HISTORY__TIME));
    }

    @Override
    public CloseableIterator<Trial> getHistory(final Deck deck) {
        String[] cols = new String[]{HISTORY__DECK, HISTORY__CARD, HISTORY__TIME, HISTORY__RESULT};
        return new CursorIterator(database.query(HISTORY,
                cols,
                HISTORY__DECK + "=?",
                new String[]{deck.getName()},
                null,
                null,
                HISTORY__TIME));
    }

    @Override
    public CloseableIterator<Trial> getHistory(final Card card) {
        String[] cols = new String[]{HISTORY__DECK, HISTORY__CARD, HISTORY__TIME, HISTORY__RESULT};
        return new CursorIterator(database.query(HISTORY,
                cols,
                HISTORY__DECK + "=? AND " + HISTORY__CARD + "=?",
                new String[]{card.getDeck().getName(), card.getName()},
                null,
                null,
                HISTORY__TIME));
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
            String deckName = cursor.getString(0);
            String cardName = cursor.getString(1);
            //System.out.println(cardName);
            long time = Long.valueOf(cursor.getString(2));
            //long time = (long) cursor.getInt(2);
            Trial.Result result = Trial.Result.valueOf(cursor.getString(3));
            Trial t = new Trial(deckName, cardName, time, result);

            cursor.moveToNext();
            return t;
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }
}
