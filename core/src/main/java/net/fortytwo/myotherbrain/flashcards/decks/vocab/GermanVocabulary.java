package net.fortytwo.myotherbrain.flashcards.decks.vocab;

import net.fortytwo.myotherbrain.flashcards.decks.InformationSource;
import net.fortytwo.myotherbrain.flashcards.db.CardStore;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;

/**
 * User: josh
 * Date: 3/19/11
 * Time: 8:20 PM
 */
public class GermanVocabulary extends VocabularyDeck {
    public GermanVocabulary(final Format format,
                            final CardStore<String, String> store) throws IOException {
        super("german_vocabulary", "German vocabulary", Locale.GERMAN, format, store);
    }

    @Override
    protected Dictionary createVocabulary() throws IOException {
        Dictionary dict = new Dictionary(locale);

        InformationSource omegaWiki = new InformationSource("OmegaWiki German-English");
        omegaWiki.setUrl("http://www.dicts.info/uddl.php");
        omegaWiki.setTimestamp("2011-03-24T07:33:00+01:00");

        InputStream is = GermanVocabulary.class.getResourceAsStream("OmegaWiki_German_English.txt");
        try {
            VocabularyParsers.parseDictsInfoList(is, dict, omegaWiki);
        } finally {
            is.close();
        }

        InformationSource wiktionary = new InformationSource("Wiktionary German-English");
        wiktionary.setUrl("http://www.dicts.info/uddl.php");
        wiktionary.setTimestamp("2011-03-24T07:35:12+01:00");

        is = GermanVocabulary.class.getResourceAsStream("Wiktionary_German_English.txt");
        try {
            VocabularyParsers.parseDictsInfoList(is, dict, wiktionary);
        } finally {
            is.close();
        }

        return dict;
    }
}
