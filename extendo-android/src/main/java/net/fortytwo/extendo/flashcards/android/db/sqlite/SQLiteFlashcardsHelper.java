package net.fortytwo.extendo.flashcards.android.db.sqlite;

import android.content.Context;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SQLiteFlashcardsHelper extends SQLiteOpenHelper {
    private static final String DATABASE_NAME = "flashcards.db";
    public static final int CURRENT_VERSION = 2;

    public SQLiteFlashcardsHelper(final Context context) {
        super(context, DATABASE_NAME, null, CURRENT_VERSION);
    }

    @Override
    public void onCreate(final SQLiteDatabase db) {
        createHistoryTable(db);
        createCardsTable(db);
    }

    private void createHistoryTable(final SQLiteDatabase db) {
        db.execSQL("CREATE TABLE " + SQLiteGameHistory.HISTORY + " (" +
                SQLiteGameHistory.HISTORY__ID + " INTEGER PRIMARY KEY AUTOINCREMENT, " +
                SQLiteGameHistory.HISTORY__DECK + " TEXT NOT NULL, " +
                SQLiteGameHistory.HISTORY__CARD + " TEXT NOT NULL, " +
                // Note: due to precision issues, timestamps are stored as strings rather than integers
                // This does not affect ORDER BY behavior, except perhaps in terms of performance.
                SQLiteGameHistory.HISTORY__TIME + "  TEXT NOT NULL, " +
                SQLiteGameHistory.HISTORY__RESULT + " TEXT NOT NULL)");
    }

    private void createCardsTable(final SQLiteDatabase db) {
        db.execSQL("CREATE TABLE " + SQLiteGameHistory.CARDS + " (" +
                SQLiteGameHistory.CARDS__ID + " INTEGER PRIMARY KEY AUTOINCREMENT, " +
                SQLiteGameHistory.CARDS__DECK + " TEXT NOT NULL, " +
                SQLiteGameHistory.CARDS__CARD + " TEXT NOT NULL, " +
                SQLiteGameHistory.CARDS__DATA + " TEXT NOT NULL)");
    }

    @Override
    public void onUpgrade(final SQLiteDatabase db,
                          final int oldVersion,
                          final int newVersion) {
        if (oldVersion < 2 && newVersion >= 2) {
            createCardsTable(db);
        }
    }
}
