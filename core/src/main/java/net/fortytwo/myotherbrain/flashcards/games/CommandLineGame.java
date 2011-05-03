package net.fortytwo.myotherbrain.flashcards.games;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;
import net.fortytwo.myotherbrain.flashcards.Game;
import net.fortytwo.myotherbrain.flashcards.GameplayException;
import net.fortytwo.myotherbrain.flashcards.Pile;
import net.fortytwo.myotherbrain.flashcards.PriorityPile;
import net.fortytwo.myotherbrain.flashcards.Trial;
import net.fortytwo.myotherbrain.flashcards.db.CardStore;
import net.fortytwo.myotherbrain.flashcards.db.CloseableIterator;
import net.fortytwo.myotherbrain.flashcards.db.file.FileBasedGameHistory;
import net.fortytwo.myotherbrain.flashcards.db.GameHistory;
import net.fortytwo.myotherbrain.flashcards.db.memory.MemoryCardStore;
import net.fortytwo.myotherbrain.flashcards.decks.geo.InternationalBorders;
import net.fortytwo.myotherbrain.flashcards.decks.tech.HttpStatusCodes;
import net.fortytwo.myotherbrain.flashcards.decks.vocab.FrenchVocabulary;
import net.fortytwo.myotherbrain.flashcards.decks.vocab.GermanVocabulary;
import net.fortytwo.myotherbrain.flashcards.decks.vocab.HSK4ChineseCharacters;
import net.fortytwo.myotherbrain.flashcards.decks.vocab.HSK4ChineseCompounds;
import net.fortytwo.myotherbrain.flashcards.decks.vocab.NPCRChineseVocabulary;
import net.fortytwo.myotherbrain.flashcards.decks.geo.NationalCapitals;
import net.fortytwo.myotherbrain.flashcards.decks.geo.USStateBorders;
import net.fortytwo.myotherbrain.flashcards.decks.vocab.SwedishVocabulary;
import net.fortytwo.myotherbrain.flashcards.decks.vocab.VocabularyDeck;

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
    public CommandLineGame(final Pile<String, String> pile,
                           final GameHistory history) {
        super(pile, history);
    }

    private void showCardHistory(final Card c) {
        CloseableIterator<Trial> h = history.getHistory(c);
        try {
            while (h.hasNext()) {
                System.out.println("\t\t" + h.next().printPlainText());
            }
        } finally {
            h.close();
        }
    }

    private boolean tryCard(final Card c,
                            final BufferedReader br,
                            final int line) throws IOException {
        //PrintStream ps = System.out;
        PrintStream ps = new PrintStream(System.out, true, "UTF-8");
        ps.println("[" + c.getDeck().getLabel() + "]");
        ps.println(line + ") " + c.getQuestion());

        while (true) {
            ps.print('\t');
            String input = br.readLine();
            if (0 < input.length()) {
                switch (input.charAt(0)) {
                    case 'r':
                        ps.println(c.getAnswer());
                        return true;
                    case 'w':
                        ps.println(c.getAnswer());
                        return false;
                    case 'p':
                        ps.println(c.getAnswer());
                        break;
                    case 'h':
                        showCardHistory(c);
                        break;
                    case 'i':
                        ps.println(showQueue(Deck.Format.TEXT));
                        break;
                    case 'q':
                        System.exit(0);
                        break;
                    default:
                        ps.print("(r)ight / (w)rong / (p)eek / (h)istory / (i)nfo / (q)uit ?  ");
                }
            }
        }
    }

    public void play() throws GameplayException {
        int line = 0;
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        while (true) {
            Card<String, String> c = drawCard();
            try {
                boolean r = tryCard(c, br, ++line);

                this.logAndReplace(c, r ? Trial.Result.Correct : Trial.Result.Incorrect);
            } catch (IOException e) {
                throw new GameplayException(e);
            }
        }
    }

    public static void main(final String[] args) {
        try {
            VocabularyDeck.Format f = Deck.Format.TEXT;

            Deck<String, String> stateBorders = new USStateBorders();
            Deck<String, String> nationalCapitals = new NationalCapitals(f);
            Deck<String, String> internationalBorders = new InternationalBorders(f);

            CardStore<String, String> store = new MemoryCardStore<String, String>();

            Deck<String, String> npcrVocabulary = new NPCRChineseVocabulary(f, store);
            Deck<String, String> hsk4Characters = new HSK4ChineseCharacters(f, store);
            Deck<String, String> hsk4Compounds = new HSK4ChineseCompounds(f, store);
            Deck<String, String> frenchVocabulary = new FrenchVocabulary(f, store);
            Deck<String, String> germanVocabulary = new GermanVocabulary(f, store);
            Deck<String, String> swedishVocabulary = new SwedishVocabulary(f, store);

            Deck<String, String> httpStatusCodes = new HttpStatusCodes(f, store);

            //Pile<String, String> pile = new SingleDeckPile<String, String>(d);

            PriorityPile<String, String> pile = new PriorityPile<String, String>();
            //*
            //pile.addDeck(stateBorders, 1);
            pile.addDeck(nationalCapitals, 1000);
            pile.addDeck(internationalBorders, 1);
            //pile.addDeck(npcrVocabulary, 4);
            pile.addDeck(hsk4Compounds, 5);
            pile.addDeck(hsk4Characters, 10);
            //*/
            pile.addDeck(frenchVocabulary, 10);
            pile.addDeck(germanVocabulary, 10);
            pile.addDeck(swedishVocabulary, 10);
            pile.addDeck(httpStatusCodes, 5);

            GameHistory h = new FileBasedGameHistory(new File("/tmp/tmpflashcards.txt"));

            new CommandLineGame(pile, h).play();
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
