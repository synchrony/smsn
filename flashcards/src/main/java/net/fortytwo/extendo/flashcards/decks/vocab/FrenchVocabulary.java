package net.fortytwo.extendo.flashcards.decks.vocab;

import net.fortytwo.extendo.flashcards.decks.InformationSource;
import net.fortytwo.extendo.flashcards.db.CardStore;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;

/**
 * User: josh
 * Date: 3/19/11
 * Time: 8:20 PM
 */
public class FrenchVocabulary extends VocabularyDeck {
    public FrenchVocabulary(final Format format,
                            final CardStore<String, String> store) throws IOException {
        super("french_vocabulary", "French vocabulary", Locale.FRENCH, format, store);
    }

    @Override
    protected Dictionary createVocabulary() throws IOException {
        Dictionary dict = new Dictionary(locale);

        InformationSource omegaWiki = new InformationSource("OmegaWiki French-English");
        omegaWiki.setUrl("http://www.dicts.info/uddl.php");
        omegaWiki.setTimestamp("2011-03-22T11:10:01+01:00");

        InputStream is = FrenchVocabulary.class.getResourceAsStream("OmegaWiki_French_English.txt");
        try {
            VocabularyParsers.parseDictsInfoList(is, dict, omegaWiki);
        } finally {
            is.close();
        }

        InformationSource wiktionary = new InformationSource("Wiktionary French-English");
        wiktionary.setUrl("http://www.dicts.info/uddl.php");
        wiktionary.setTimestamp("2011-03-23T11:21:30+01:00");

        is = FrenchVocabulary.class.getResourceAsStream("Wiktionary_French_English.txt");
        try {
            VocabularyParsers.parseDictsInfoList(is, dict, wiktionary);
        } finally {
            is.close();
        }

        return dict;
    }

    public static void main(final String[] args) throws IOException {
        /*
        CardStore<String, String> store = new MemoryCardStore<String, String>();
        FrenchVocabulary deck = new FrenchVocabulary(Format.HTML, store);
        for (Card card : deck.getCards()) {
            if (card.getName().startsWith("zone pi")) {
                System.out.println(card.getName() + ":");
                System.out.println("\t" + card.getAnswer());
            }
        }*/
    }
}
