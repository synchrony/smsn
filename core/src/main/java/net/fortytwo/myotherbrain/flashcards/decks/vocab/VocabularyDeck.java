package net.fortytwo.myotherbrain.flashcards.decks.vocab;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;
import net.fortytwo.myotherbrain.flashcards.db.CardStore;
import net.fortytwo.myotherbrain.flashcards.db.CloseableIterator;

import java.io.IOException;
import java.util.List;
import java.util.Locale;

/**
 * A deck of vocabulary terms with pronunciation and meaning.
 * <p/>
 * User: josh
 * Date: 3/9/11
 * Time: 6:04 PM
 */
public abstract class VocabularyDeck extends Deck<String, String> {

    protected final Locale locale;
    private final CardStore<String, String> store;

    public VocabularyDeck(final String name,
                          final String label,
                          final Locale locale,
                          final Format format,
                          final CardStore<String, String> store) throws IOException {
        super(name, label);

        this.store = store;
        this.locale = locale;

        Dictionary d = createVocabulary();
        for (String key : d.getKeys()) {
            List<Term> defs = d.getDefinitions(key);

            String n = Card.findCardName(key);
            store.add(new VocabularyCard(n, this, defs, format));
        }
    }

    protected abstract Dictionary createVocabulary() throws IOException;

    @Override
    public CloseableIterator<Card<String, String>> getCards() {
        return store.findAll(this);
    }

    public Card<String, String> getCard(final String name) {
        return store.find(this, name);
    }
}
