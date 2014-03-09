package net.fortytwo.extendo.flashcards.decks.vocab;

import net.fortytwo.extendo.flashcards.decks.InformationSource;
import net.fortytwo.extendo.flashcards.db.CardStore;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NPCRChineseVocabulary extends VocabularyDeck {

    public NPCRChineseVocabulary(final Format format,
                            final CardStore<String, String> store) throws IOException {
        super("npcr_vocabulary", "NPCR Chinese vocabulary", Locale.CHINESE, format, store);
    }

    @Override
    protected Dictionary createVocabulary() throws IOException {
        Dictionary dict = new Dictionary(locale);

        InformationSource source = new InformationSource("NPCR");
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
