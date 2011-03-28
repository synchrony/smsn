package net.fortytwo.myotherbrain.flashcards.decks.vocab;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;

/**
 * User: josh
 * Date: 3/9/11
 * Time: 6:04 PM
 */
public class NPCRChineseVocabulary extends VocabularyDeck {

    public NPCRChineseVocabulary(final Format format) throws IOException {
        super("npcr_vocabulary", "NPCR Chinese vocabulary", Locale.CHINESE, format);
    }

    @Override
    protected Dictionary createVocabulary() throws IOException {
        Dictionary dict = new Dictionary(locale);

        VocabularySource source = new VocabularySource("NPCR");
        source.setComment("New Practical Chinese Reader vocabulary list");
        source.setUrl("http://hskflashcards.com/download.php");
        source.setTimestamp("2011-3-17");

        InputStream is = NPCRChineseVocabulary.class.getResourceAsStream("NPCR_vocabulary.txt");
        try {
            VocabularyParsers.parseNPCRList(is, dict, source);
        } finally {
            is.close();
        }

        return dict;
    }
}
