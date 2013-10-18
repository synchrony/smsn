package net.fortytwo.extendo.flashcards.android;

import android.app.Activity;
import android.content.Intent;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.webkit.WebView;
import android.widget.RelativeLayout;
import net.fortytwo.extendo.R;
import net.fortytwo.extendo.flashcards.Card;
import net.fortytwo.extendo.flashcards.Deck;
import net.fortytwo.extendo.flashcards.GameplayException;
import net.fortytwo.extendo.flashcards.PriorityPile;
import net.fortytwo.extendo.flashcards.Trial;
import net.fortytwo.extendo.flashcards.android.db.sqlite.SQLiteFlashcardsHelper;
import net.fortytwo.extendo.flashcards.android.db.sqlite.SQLiteGameHistory;
import net.fortytwo.extendo.flashcards.db.CardStore;
import net.fortytwo.extendo.flashcards.db.CloseableIterator;
import net.fortytwo.extendo.flashcards.db.GameHistory;
import net.fortytwo.extendo.flashcards.db.file.FileBasedGameHistory;
import net.fortytwo.extendo.flashcards.db.memory.MemoryCardStore;
import net.fortytwo.extendo.flashcards.decks.SimpleDeck;
import net.fortytwo.extendo.flashcards.decks.geo.InternationalBorders;
import net.fortytwo.extendo.flashcards.decks.geo.NationalCapitals;
import net.fortytwo.extendo.flashcards.decks.tech.HttpStatusCodes;
import net.fortytwo.extendo.flashcards.decks.vocab.FrenchVocabulary;
import net.fortytwo.extendo.flashcards.decks.vocab.HSK4ChineseCharacters;
import net.fortytwo.extendo.flashcards.decks.vocab.HSK4ChineseCompounds;
import net.fortytwo.extendo.flashcards.decks.vocab.VocabularyDeck;
import net.fortytwo.extendo.flashcards.games.AsynchronousGame;

import java.io.File;
import java.io.IOException;

public class Flashcards4Android extends Activity {
    public static final String INFO = "net.fortytwo.extendo.flashcards.android.info";

    public static final String HTML_PREFIX = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
            "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n" +
            "        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" +
            "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n" +
            "<head>\n" +
            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"flashcards.css\"/>\n" +
            "</head>\n" +
            "<body>\n";
    public static final String HTML_SUFFIX = "</body>\n" +
            "</html>";

    private RelativeLayout questionFace;
    private RelativeLayout answerFace;
    private WebView questionText;
    private WebView answerText;

    private static Flashcards4Android activity;
    private static AsynchronousGame game;
    private static SQLiteDatabase db;
    private static GameHistory history;

    @Override
    public void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        activity = this;

        setContentView(R.layout.flashcards_layout);

        questionFace = (RelativeLayout) findViewById(R.id.questionFace);
        questionText = (WebView) findViewById(R.id.questionText);
        // This is one way of getting rid of the white margin otherwise visible on the right of the WebView.
        questionText.setScrollBarStyle(WebView.SCROLLBARS_OUTSIDE_OVERLAY);
        questionText.setOnTouchListener(questionTouched);

        answerFace = (RelativeLayout) findViewById(R.id.answerFace);
        answerText = (WebView) findViewById(R.id.answerText);
        answerText.setScrollBarStyle(WebView.SCROLLBARS_OUTSIDE_OVERLAY);

        findViewById(R.id.correct).setOnClickListener(correct);
        findViewById(R.id.incorrect).setOnClickListener(incorrect);

        SQLiteOpenHelper openHelper = new SQLiteFlashcardsHelper(this);

        try {
            // Create the game only once.  In subsequent instances of this activity, re-use it.
            if (null == db) {
                db = openHelper.getWritableDatabase();
                game = createGame(db);
            }

            game.play();
        } catch (Exception e) {
            //throw new IllegalStateException(e);
            e.printStackTrace(System.err);
        }
    }

    private Flashcards4Android getCurrentActivity() {
        return activity;
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        super.onCreateOptionsMenu(menu);

        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.flashcards_menu, menu);

        return true;
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        super.onPrepareOptionsMenu(menu);

        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.info:
                //System.out.println("info_layout!");
                Intent i = new Intent(this, FlashcardsInfo.class);
                Bundle b = new Bundle();
                b.putString(INFO, game.showQueue(Deck.Format.HTML));
                i.putExtras(b);
                startActivity(i);
                return true;
            case R.id.save:
                saveHistory();
                return true;
            case R.id.settings:
                startActivity(new Intent(this, FlashcardsSettings.class));
                //System.out.println("settings!");
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    @Override
    public void onDestroy() {
        //db.close();
        super.onDestroy();
    }

    private void saveHistory() {
        try {
            FileBasedGameHistory h = new FileBasedGameHistory(
                    // TODO: choose a more generic location
                    new File("/sdcard/42/flashcards-history.txt"));
            try {
                h.clear();

                CloseableIterator<Trial> i = history.getHistory();
                try {
                    while (i.hasNext()) {
                        h.log(i.next());
                    }
                } finally {
                    i.close();
                }
            } finally {
                h.close();
            }
        } catch (IOException e) {
            e.printStackTrace(System.err);
        }
    }

    private View.OnClickListener correct = new View.OnClickListener() {
        public void onClick(final View v) {
            try {
                game.correct();
            } catch (GameplayException e) {
                e.printStackTrace(System.err);
            }
        }
    };

    private View.OnClickListener incorrect = new View.OnClickListener() {
        public void onClick(final View v) {
            try {
                game.incorrect();
            } catch (GameplayException e) {
                e.printStackTrace(System.err);
            }
        }
    };

    private View.OnTouchListener questionTouched = new View.OnTouchListener() {
        public boolean onTouch(final View view,
                               final MotionEvent motionEvent) {
            enterAnswerMode();
            return true;
        }
    };

    private void enterQuestionMode() {
        answerFace.setVisibility(View.GONE);
        questionFace.setVisibility(View.VISIBLE);
    }

    private void enterAnswerMode() {
        questionFace.setVisibility(View.GONE);
        answerFace.setVisibility(View.VISIBLE);
    }

    private AsynchronousGame createGame(final SQLiteDatabase db) throws IOException {
        VocabularyDeck.Format f = Deck.Format.HTML;

        //Deck<String, String> stateBorders = new USStateBorders();
        Deck<String, String> nationalCapitals = new NationalCapitals(f);
        Deck<String, String> internationalBorders = new InternationalBorders(f);
        //Deck<String, String> npcrVocabulary = new NPCRVocabulary();

        CardStore<String, String> store = new MemoryCardStore<String, String>();
        //CardStore<String, String> store = new SQLiteCardStore<String, String>(db, new VocabularySerializer(f));
        store.clear();

        Deck<String, String> hsk4Characters = new HSK4ChineseCharacters(f, store);
        Deck<String, String> hsk4Compounds = new HSK4ChineseCompounds(f, store);
        Deck<String, String> frenchVocab = new FrenchVocabulary(f, store);
        //Deck<String, String> germanVocab = new GermanVocabulary(f, store);
        //Deck<String, String> swedishVocab = new SwedishVocabulary(f, store);

        Deck<String, String> httpStatusCodes = new HttpStatusCodes(f, store);

        SimpleDeck misc = new SimpleDeck(f, "miscellaneous", "miscellaneous");
        misc.addCard("meaningless random number",
                "What is the value of the meaningless random number generated on 2011-03-28?",
                "0106314906");

        //Pile<String, String> pile = new SingleDeckPile<String, String>(d);

        PriorityPile<String, String> pile = new PriorityPile<String, String>();
        //pile.addDeck(stateBorders, 1);
        pile.addDeck(nationalCapitals, 1);
        pile.addDeck(internationalBorders, 1);
        //pile.addDeck(npcrVocabulary, 4);
        pile.addDeck(frenchVocab, 4);
        //pile.addDeck(germanVocab, 4);
        //pile.addDeck(swedishVocab, 4);
        pile.addDeck(hsk4Compounds, 4);
        pile.addDeck(hsk4Characters, 8);
        pile.addDeck(misc, 5);
        pile.addDeck(httpStatusCodes, 3);

        history = new SQLiteGameHistory(db);

        return new AsynchronousGame<String, String>(pile, history) {
            @Override
            public void nextQuestion(final Card<String, String> current) {
                Flashcards4Android a = getCurrentActivity();
                a.showQuestion(current);
                a.showAnswer(current);
                a.enterQuestionMode();
            }
        };
    }

    private void showQuestion(final Card<String, String> card) {
        String text = HTML_PREFIX + card.getQuestion() + HTML_SUFFIX;
        questionText.loadDataWithBaseURL("file:///android_asset/", text, "application/xhtml+xml", "utf-8", null);
    }

    private void showAnswer(final Card<String, String> card) {
        String text = HTML_PREFIX + card.getAnswer() + HTML_SUFFIX;
        answerText.loadDataWithBaseURL("file:///android_asset/", text, "application/xhtml+xml", "utf-8", null);
    }
    /*
    private String htmlEscape(final String s) {
        StringBuilder sb = new StringBuilder();
        for (char c : s.toCharArray()) {
            int i = (int) c;
            if (i > 255) {
                sb.append("&#x").append(i).append(";");
            } else {
                sb.append(c);
            }
        }

        System.out.println("converted '" + s + "' to '" + sb.toString() + "'");
        return sb.toString();
    }   */
}
