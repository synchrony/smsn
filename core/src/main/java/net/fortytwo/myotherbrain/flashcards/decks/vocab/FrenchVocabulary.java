package net.fortytwo.myotherbrain.flashcards.decks.vocab;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;

/**
 * User: josh
 * Date: 3/19/11
 * Time: 8:20 PM
 */
public class FrenchVocabulary extends VocabularyDeck {
    public FrenchVocabulary() throws IOException {
        super("french_vocabulary");
    }

    @Override
    public Dictionary createVocabulary() throws IOException {
        Dictionary dict = new Dictionary();

        VocabularySource omegaWiki = new VocabularySource("OmegaWiki");
        omegaWiki.setUrl("http://www.dicts.info/uddl.php");
        omegaWiki.setTimestamp("2011-03-22T11:10:01+01:00");

        InputStream is = FrenchVocabulary.class.getResourceAsStream("OmegaWiki_French_English.txt");
        try {
            VocabularyParsers.parseDictsInfoList(is, dict, Locale.FRENCH, omegaWiki);
        } finally {
            is.close();
        }

        VocabularySource wiktionary = new VocabularySource("Wiktionary");
        wiktionary.setUrl("http://www.dicts.info/uddl.php");
        wiktionary.setTimestamp("2011-03-23T11:21:30+01:00");

        is = FrenchVocabulary.class.getResourceAsStream("Wiktionary_French_English.txt");
        try {
            VocabularyParsers.parseDictsInfoList(is, dict, Locale.FRENCH, wiktionary);
        } finally {
            is.close();
        }

        return dict;
    }
}
