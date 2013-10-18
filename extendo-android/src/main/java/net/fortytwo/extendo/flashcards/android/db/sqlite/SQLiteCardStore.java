package net.fortytwo.extendo.flashcards.android.db.sqlite;

import android.content.ContentValues;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import net.fortytwo.extendo.flashcards.Card;
import net.fortytwo.extendo.flashcards.Deck;
import net.fortytwo.extendo.flashcards.db.CardSerializer;
import net.fortytwo.extendo.flashcards.db.CardStore;
import net.fortytwo.extendo.flashcards.db.CloseableIterator;

import java.io.IOException;

/**
 * User: josh
 * Date: 3/29/11
 * Time: 8:21 PM
 */
public class SQLiteCardStore<Q, A> implements CardStore<Q, A> {
    private final SQLiteDatabase database;
    private final CardSerializer<Q, A> serializer;

    public SQLiteCardStore(final SQLiteDatabase database,
                           final CardSerializer<Q, A> serializer) {
        this.serializer = serializer;
        this.database = database;
    }

    public void add(final Card<Q, A> card) throws IOException {
        String data = serializer.serialize(card);

        ContentValues cv = new ContentValues();
        cv.put(SQLiteGameHistory.CARDS__DECK, card.getDeck().getName());
        cv.put(SQLiteGameHistory.CARDS__CARD, card.getName());
        cv.put(SQLiteGameHistory.CARDS__DATA, data);
        database.insert(SQLiteGameHistory.CARDS, SQLiteGameHistory.CARDS__ID, cv);
    }

    public Card<Q, A> find(final Deck<Q, A> deck,
                           final String cardName) {
        String[] cols = new String[]{SQLiteGameHistory.CARDS__DECK, SQLiteGameHistory.CARDS__CARD, SQLiteGameHistory.CARDS__DATA};
        CloseableIterator<Card<Q, A>> iter = new CursorIterator(database.query(SQLiteGameHistory.CARDS,
                cols,
                SQLiteGameHistory.CARDS__DECK + "=?",
                new String[]{deck.getName()},
                null,
                null,
                null), deck);
        try {
            if (iter.hasNext()) {
                return iter.next();
            }
        } finally {
            iter.close();
        }

        return null;
    }

    public CloseableIterator<Card<Q, A>> findAll(final Deck<Q, A> deck) {
        String[] cols = new String[]{SQLiteGameHistory.CARDS__DECK, SQLiteGameHistory.CARDS__CARD, SQLiteGameHistory.CARDS__DATA};
        return new CursorIterator(database.query(SQLiteGameHistory.CARDS,
                cols,
                SQLiteGameHistory.CARDS__DECK + "=?",
                new String[]{deck.getName()},
                null,
                null,
                null), deck);
    }

    public void clear() {
        database.delete(SQLiteGameHistory.CARDS, null, null);
    }

    private class CursorIterator implements CloseableIterator<Card<Q, A>> {
        private final Cursor cursor;
        private final Deck<Q, A> deck;

        public CursorIterator(final Cursor cursor,
                              final Deck<Q, A> deck) {
            this.cursor = cursor;
            this.deck = deck;
            cursor.moveToFirst();
        }

        public void close() {
            cursor.close();
        }

        public boolean hasNext() {
            return !cursor.isAfterLast();
        }

        public Card<Q, A> next() {
            String deckName = cursor.getString(0);
            String cardName = cursor.getString(1);
            String data = cursor.getString(2);

            if (!deckName.equals(deck.getName())) {
                throw new IllegalStateException();
            }

            Card<Q, A> card;
            try {
                card = serializer.deserialize(cardName, deck, data);
            } catch (IOException e) {
                throw new IllegalStateException(e);
            }

            cursor.moveToNext();
            return card;
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }
}
