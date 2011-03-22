package net.fortytwo.myotherbrain.flashcards.games.android;

import android.app.Activity;
import android.content.Intent;
import android.database.sqlite.SQLiteDatabase;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.webkit.WebView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import net.fortytwo.myotherbrain.R;
import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;
import net.fortytwo.myotherbrain.flashcards.Game;
import net.fortytwo.myotherbrain.flashcards.GameplayException;
import net.fortytwo.myotherbrain.flashcards.Pile;
import net.fortytwo.myotherbrain.flashcards.PriorityPile;
import net.fortytwo.myotherbrain.flashcards.Trial;
import net.fortytwo.myotherbrain.flashcards.db.GameHistory;
import net.fortytwo.myotherbrain.flashcards.db.sqlite.SQLiteGameHistory;
import net.fortytwo.myotherbrain.flashcards.db.sqlite.SQLiteGameHistoryHelper;
import net.fortytwo.myotherbrain.flashcards.decks.geo.InternationalBorders;
import net.fortytwo.myotherbrain.flashcards.decks.vocab.HSK4Characters;
import net.fortytwo.myotherbrain.flashcards.decks.vocab.HSK4Compounds;
import net.fortytwo.myotherbrain.flashcards.decks.geo.NationalCapitals;

import java.io.IOException;

public class Flashcards4Android extends Activity {
    public static final String INFO = "flashcards_info_layout";

    private RelativeLayout questionFace;
    private RelativeLayout answerFace;
    private WebView questionText;
    private WebView answerText;
    private TextView debugText;

    private AndroidGame game;
    private SQLiteDatabase db;

    @Override
    public void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.flashcards_layout);

        questionFace = (RelativeLayout) findViewById(R.id.questionFace);
        questionText = (WebView) findViewById(R.id.questionText);
        // This is one way of getting rid of the white margin otherwise visible on the right of the WebView.
        questionText.setScrollBarStyle(WebView.SCROLLBARS_OUTSIDE_OVERLAY);
        questionText.setOnTouchListener(questionTouched);

        answerFace = (RelativeLayout) findViewById(R.id.answerFace);
        answerText = (WebView) findViewById(R.id.answerText);
        answerText.setScrollBarStyle(WebView.SCROLLBARS_OUTSIDE_OVERLAY);

        debugText = (TextView) findViewById(R.id.debugText);

        findViewById(R.id.correct).setOnClickListener(correct);
        findViewById(R.id.incorrect).setOnClickListener(incorrect);

        SQLiteGameHistoryHelper openHelper = new SQLiteGameHistoryHelper(this);
        db = openHelper.getWritableDatabase();

        try {
            game = createGame(db);
            game.play();
        } catch (Exception e) {
            //throw new IllegalStateException(e);
            e.printStackTrace(System.err);
        }
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
                b.putString(INFO, game.showQueue());
                i.putExtras(b);
                startActivity(i);
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
        db.close();
        super.onDestroy();
    }

    private View.OnClickListener correct = new View.OnClickListener() {
        public void onClick(final View v) {
            try {
                game.correct();
            } catch (IOException e) {
                e.printStackTrace(System.err);
            }
        }
    };

    private View.OnClickListener incorrect = new View.OnClickListener() {
        public void onClick(final View v) {
            try {
                game.incorrect();
            } catch (IOException e) {
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

    private AndroidGame createGame(final SQLiteDatabase db) throws IOException {
        //Deck<String, String> stateBorders = new USStateBorders();
        Deck<String, String> nationalCapitals = new NationalCapitals();
        Deck<String, String> internationalBorders = new InternationalBorders();
        //Deck<String, String> npcrVocabulary = new NPCRVocabulary();
        Deck<String, String> hsk4Characters = new HSK4Characters();
        Deck<String, String> hsk4Compounds = new HSK4Compounds();

        //Pile<String, String> pile = new SingleDeckPile<String, String>(d);

        PriorityPile<String, String> pile = new PriorityPile<String, String>();
        //pile.addDeck(stateBorders, 1);
        pile.addDeck(nationalCapitals, 1);
        pile.addDeck(internationalBorders, 1);
        //pile.addDeck(npcrVocabulary, 4);
        pile.addDeck(hsk4Compounds, 2);
        pile.addDeck(hsk4Characters, 8);

        GameHistory h = new SQLiteGameHistory(db);

        return new AndroidGame(pile, h);
    }

    private class AndroidGame extends Game<String, String> {
        private Card<String, String> card;

        public AndroidGame(final Pile<String, String> pile,
                           final GameHistory history) {
            super(pile, history);
        }

        @Override
        public void play() throws GameplayException {
            nextCard();
        }

        public void nextCard() {
            debugText.setText(showQueue());
            card = drawCard();
            showQuestion(card);
            showAnswer(card);
            enterQuestionMode();
        }

        public void correct() throws IOException {
            long now = System.currentTimeMillis();
            correct(card, now);
            history.log(new Trial(card.getDeck().getName(), card.getName(), now, Trial.Result.Correct));
            replaceCard(card);
            nextCard();
        }

        public void incorrect() throws IOException {
            long now = System.currentTimeMillis();
            incorrect(card, now);
            history.log(new Trial(card.getDeck().getName(), card.getName(), now, Trial.Result.Incorrect));
            replaceCard(card);
            nextCard();
        }
    }

    private static final String HTML_PREFIX = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
            "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n" +
            "        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" +
            "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n" +
            "<head>\n" +
            "    <link rel=\"stylesheet\" type=\"text/css\" href=\"flashcards.css\"/>\n" +
            "</head>\n" +
            "<body>\n";
    private static final String HTML_SUFFIX = "</body>\n" +
            "</html>";

    private void showQuestion(final Card<String, String> card) {
        String text = HTML_PREFIX + "<p class='question'>" + htmlEscape(card.getQuestion()) + "</p>" + HTML_SUFFIX;
        questionText.loadDataWithBaseURL("file:///android_asset/", text, "application/xhtml+xml", "utf-8", null);

        // This seems to be necessary in order to refresh the view.
        //questionText.reload();
    }

    private void showAnswer(final Card<String, String> card) {
        String text = HTML_PREFIX + "<span class='answer'>" + htmlEscape(card.getAnswer()) + "</span>" + HTML_SUFFIX;
        answerText.loadDataWithBaseURL("file:///android_asset/", text, "application/xhtml+xml", "utf-8", null);

        // This seems to be necessary in order to refresh the view.
        //answerText.reload();
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

    private static String htmlEscape(String s) {
        StringBuffer sb = new StringBuffer(s.length());
        // true if last char was blank
        boolean lastWasBlankChar = false;
        int len = s.length();
        char c;

        for (int i = 0; i < len; i++) {
            c = s.charAt(i);
            if (c == ' ') {
                // blank gets extra work,
                // this solves the problem you get if you replace all
                // blanks with &nbsp;, if you do that you loss
                // word breaking
                if (lastWasBlankChar) {
                    lastWasBlankChar = false;
                    sb.append("&nbsp;");
                } else {
                    lastWasBlankChar = true;
                    sb.append(' ');
                }
            } else {
                lastWasBlankChar = false;
                //
                // HTML Special Chars
                if (c == '"')
                    sb.append("&quot;");
                else if (c == '&')
                    sb.append("&amp;");
                else if (c == '<')
                    sb.append("&lt;");
                else if (c == '>')
                    sb.append("&gt;");
                else if (c == '\n')
                    // Handle Newline
                    sb.append("&lt;br/&gt;");
                else {
                    int ci = 0xffff & c;
                    if (ci < 160)
                        // nothing special only 7 Bit
                        sb.append(c);
                    else {
                        // Not 7 Bit use the unicode system
                        sb.append("&#");
                        sb.append(Integer.toString(ci));
                        sb.append(';');
                    }
                }
            }
        }

        System.out.println("converted '" + s + "' to '" + sb.toString() + "'");
        return sb.toString();
    }
}
