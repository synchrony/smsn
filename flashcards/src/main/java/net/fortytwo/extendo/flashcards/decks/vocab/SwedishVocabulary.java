package net.fortytwo.extendo.flashcards.decks.vocab;

import net.fortytwo.extendo.flashcards.decks.InformationSource;
import net.fortytwo.extendo.flashcards.db.CardStore;

import java.io.IOException;
import java.io.InputStream;
import java.text.NumberFormat;
import java.util.Locale;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SwedishVocabulary extends VocabularyDeck {
    private static final Logger LOGGER = Logger.getLogger(SwedishVocabulary.class.getName());

    public SwedishVocabulary(final Format format,
                            final CardStore<String, String> store) throws IOException {
        super("swedish_vocabulary", "Swedish vocabulary", findSwedishLocale(), format, store);
    }

    @Override
    protected Dictionary createVocabulary() throws IOException {
        Dictionary dict = new Dictionary(locale);

        Locale locale = findSwedishLocale();
        if (null == locale) {
            LOGGER.warning("locale for Swedish vocabulary is not available.  Loading an empty dictionary.");
        } else {
            InformationSource omegaWiki = new InformationSource("OmegaWiki Swedish-English");
            omegaWiki.setUrl("http://www.dicts.info/uddl.php");
            omegaWiki.setTimestamp("2011-03-24T07:46:30+01:00");

            InputStream is = SwedishVocabulary.class.getResourceAsStream("OmegaWiki_Swedish_English.txt");
            try {
                VocabularyParsers.parseDictsInfoList(is, dict, omegaWiki);
            } finally {
                is.close();
            }

            InformationSource wiktionary = new InformationSource("Wiktionary Swedish-English");
            wiktionary.setUrl("http://www.dicts.info/uddl.php");
            wiktionary.setTimestamp("2011-03-24T07:47:47+01:00");

            is = SwedishVocabulary.class.getResourceAsStream("Wiktionary_Swedish_English.txt");
            try {
                VocabularyParsers.parseDictsInfoList(is, dict, wiktionary);
            } finally {
                is.close();
            }
        }

        return dict;
    }

    private static Locale findSwedishLocale() {
        for (Locale l : NumberFormat.getAvailableLocales()) {
            if (l.getISO3Language().equals("swe")) {
                return l;
            }
        }

        return null;
    }
}
