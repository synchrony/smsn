package net.fortytwo.myotherbrain.flashcards.db.sqlite;

import android.content.Context;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

/**
 * User: josh
 * Date: 3/5/11
 * Time: 7:13 PM
 */
public class SQLiteGameHistoryHelper extends SQLiteOpenHelper {
    private static final String DATABASE_NAME = "flashcards.db";
    public static final int CURRENT_VERSION = 1;

    public SQLiteGameHistoryHelper(final Context context) {
        super(context, DATABASE_NAME, null, CURRENT_VERSION);
    }

    @Override
    public void onCreate(final SQLiteDatabase db) {
        db.execSQL("CREATE TABLE " + SQLiteGameHistory.HISTORY + " (" +
                SQLiteGameHistory.HISTORY__ID + " INTEGER PRIMARY KEY AUTOINCREMENT, " +
                SQLiteGameHistory.HISTORY__DECK + " TEXT NOT NULL, " +
                SQLiteGameHistory.HISTORY__CARD + " TEXT NOT NULL, " +
                // Note: due to precision issues, timestamps are stored as strings rather than integers
                // This does not affect ORDER BY behavior, except perhaps in terms of performance.
                SQLiteGameHistory.HISTORY__TIME + "  TEXT NOT NULL, " +
                SQLiteGameHistory.HISTORY__RESULT + " TEXT NOT NULL)");
    }

    @Override
    public void onUpgrade(final SQLiteDatabase db,
                          final int oldVersion,
                          final int newVersion) {
        // No need to upgrade yet.
    }
}
