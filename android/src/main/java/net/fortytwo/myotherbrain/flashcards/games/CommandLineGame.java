package net.fortytwo.myotherbrain.flashcards.games;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;
import net.fortytwo.myotherbrain.flashcards.Game;
import net.fortytwo.myotherbrain.flashcards.GameplayException;
import net.fortytwo.myotherbrain.flashcards.Trial;
import net.fortytwo.myotherbrain.flashcards.db.CloseableIterator;
import net.fortytwo.myotherbrain.flashcards.db.FileBasedGameHistory;
import net.fortytwo.myotherbrain.flashcards.db.GameHistory;
import net.fortytwo.myotherbrain.flashcards.decks.NPCRVocabulary;
import net.fortytwo.myotherbrain.flashcards.decks.NationalCapitals;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;

/**
 * User: josh
 * Date: 3/14/11
 * Time: 11:58 AM
 */
public class CommandLineGame extends Game<String, String> {
    public CommandLineGame(final Deck<String, String> deck,
                           final GameHistory history) {
        super(deck, history);
    }

    private void showCardHistory(final Card c) {
        CloseableIterator<Trial> h = history.getHistory(deck, c);
        try {
            while (h.hasNext()) {
                System.out.println("\t\t" + h.next().tabDelimited());
            }
        } finally {
            h.close();
        }
    }

    private boolean tryCard(final Card c,
                            final BufferedReader br,
                            final int line) throws IOException {
        PrintStream ps = System.out;
        //PrintStream ps = new PrintStream(System.out, true, "UTF-8");
        ps.println("" + line + ") " + deck.getQuestion(c));
        showCardHistory(c);

        while (true) {
            showQueue();
            String input = br.readLine();
            switch (input.charAt(0)) {
                case 'r':
                    ps.println("\t" + deck.getAnswer(c));
                    return true;
                case 'w':
                    ps.println("\t" + deck.getAnswer(c));
                    return false;
                case 'e':
                    ps.println("\t" + deck.getAnswer(c));
                    break;
                case 'q':
                    System.exit(0);
                    break;
                default:
                    ps.print("r/w/e/q?  ");
            }
        }
    }

    public void play() throws GameplayException {
        int line = 0;
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        while (true) {
            Card c = drawCard();
            try {
                long now = System.currentTimeMillis();

                if (tryCard(c, br, ++line)) {
                    c.correct(now);
                    history.log(new Trial(deck.getName(), c.getName(), now, Trial.Result.Correct));
                } else {
                    c.incorrect(now);
                    history.log(new Trial(deck.getName(), c.getName(), now, Trial.Result.Incorrect));
                }
            } catch (IOException e) {
                throw new GameplayException(e);
            }
            replaceCard(c);
        }
    }

    public static void main(final String[] args) {
        try {
            //Deck<String, String> d = new USStateBorders();
            //Deck<String, String> d = new NationalCapitals();
            Deck<String, String> d = new NPCRVocabulary();
            GameHistory h = new FileBasedGameHistory(new File("/tmp/flashcards_tmp.txt"));

            new CommandLineGame(d, h).play();
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
