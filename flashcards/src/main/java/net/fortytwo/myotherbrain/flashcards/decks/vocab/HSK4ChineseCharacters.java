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
public class HSK4ChineseCharacters extends VocabularyDeck {
    public HSK4ChineseCharacters(final Format format,
                            final CardStore<String, String> store) throws IOException {
        super("hsk4_characters", "HSK Chinese characters", Locale.CHINESE, format, store);
    }

    @Override
    protected Dictionary createVocabulary() throws IOException {
        Dictionary dict = new Dictionary(locale);

        InformationSource source = new InformationSource("renzhe's lists");
        source.setComment("HSK level 4 vocabulary lists compiled by user renzhe");
        source.setUrl("http://www.chinese-forums.com/index.php?/topic/14829-hsk-character-lists/");
        source.setTimestamp("2011-3-19");

        InputStream is = HSK4ChineseCharacters.class.getResourceAsStream("hsk4-allchars-tab.txt");
        try {
            VocabularyParsers.parseHSK4List(is, dict, source);
        } finally {
            is.close();
        }

        return dict;
    }
}
