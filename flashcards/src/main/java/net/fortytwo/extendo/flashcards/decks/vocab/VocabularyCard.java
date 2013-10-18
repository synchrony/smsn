package net.fortytwo.extendo.flashcards.decks.vocab;

import net.fortytwo.extendo.flashcards.Card;
import net.fortytwo.extendo.flashcards.Deck;
import net.fortytwo.extendo.flashcards.decks.Answer;
import net.fortytwo.extendo.flashcards.decks.AnswerFormatter;
import net.fortytwo.extendo.flashcards.decks.QuestionFormatter;

import java.util.List;

/**
 * User: josh
 * Date: 3/29/11
 * Time: 11:25 PM
 */
class VocabularyCard extends Card<String, String> {
    private final List<Term> definitions;
    private VocabularyDeck.Format format;

    public VocabularyCard(final String name,
                          final Deck deck,
                          final List<Term> definitions,
                          final VocabularyDeck.Format format) {
        super(name, deck);
        this.definitions = definitions;
        this.format = format;
    }

    public List<Term> getDefinitions() {
        return definitions;
    }

    @Override
    public String getQuestion() {
        String question = definitions.get(0).getForms().get(0) + " = ?";

        QuestionFormatter f = new QuestionFormatter(deck, format);
        f.setQuestion(question);
        return f.format();
    }

    @Override
    public String getAnswer() {
        AnswerFormatter f = new AnswerFormatter(format);

        for (Term t : definitions) {
            Answer a = new Answer();
            f.addAnswer(a);
            if (null != t.getSource()) {
                a.setSource(t.getSource());
            }

            for (String form : t.getForms()) {
                a.addForm(form);
            }

            a.setPronuncation(t.getPronunciation());
            a.setType(t.getType());
            a.setMeaning(t.getMeaning());
        }

        return f.format();
    }

    @Override
    public String toString() {
        return definitions.get(0).getForms().get(0);
    }
}
