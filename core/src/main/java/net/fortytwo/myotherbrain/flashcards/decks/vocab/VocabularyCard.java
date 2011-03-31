package net.fortytwo.myotherbrain.flashcards.decks.vocab;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;

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
        StringBuilder sb = new StringBuilder();

        if (VocabularyDeck.Format.HTML == format) {
            sb.append("<div class=\"question\">\n");

            // Context
            sb.append("<div class=\"question-context\">").append(deck.getLabel()).append("</div>\n");
        }

        String question = VocabularyDeck.Format.HTML == format
                ? VocabularyDeck.htmlEscape(definitions.get(0).getForms().get(0)) + " =&nbsp;?"
                : definitions.get(0).getForms().get(0) + " = ?";
        sb.append(question);

        if (VocabularyDeck.Format.HTML == format) {
            sb.append("</div>\n");
        }

        return sb.toString();
    }

    @Override
    public String getAnswer() {
        StringBuilder sb = new StringBuilder();

        if (VocabularyDeck.Format.HTML == format) {
            sb.append("<div class=\"answer\">\n");
        }

        for (Term t : definitions) {
            // Source
            if (null != t.getSource()) {
                String label = t.getSource().getLabel();
                if (VocabularyDeck.Format.HTML == format) {
                    sb.append("<div class=\"answer-source\">");
                    label = VocabularyDeck.htmlEscape(label);
                } else {
                    sb.append("[");
                }

                if (VocabularyDeck.Format.HTML == format && null != t.getSource().getUrl()) {
                    sb.append("<a href=\"").append(t.getSource().getUrl()).append("\">");
                }
                sb.append(label);
                if (VocabularyDeck.Format.HTML == format) {
                    if (null != t.getSource().getUrl()) {
                        sb.append("</a>");
                    }
                } else {
                    sb.append("]\n");
                }

                if (VocabularyDeck.Format.HTML == format) {
                    sb.append("</div>\n");
                }
            }

            if (VocabularyDeck.Format.HTML == format) {
                sb.append("<div class=\"answer-definition\">");
            }

            // Primary form;
            String primaryForm = t.getForms().get(0);
            if (VocabularyDeck.Format.HTML == format) {
                primaryForm = VocabularyDeck.htmlEscape(primaryForm);
                sb.append("<span class=\"answer-primary-form\">");
            }
            sb.append(primaryForm);
            if (VocabularyDeck.Format.HTML == format) {
                sb.append("</span>");
            }

            // Secondary forms
            if (2 <= t.getForms().size()) {
                if (VocabularyDeck.Format.HTML == format) {
                    sb.append("<span class=\"answer-secondary-forms\">");
                }
                sb.append(" (");
                for (int i = 1; i < t.getForms().size(); i++) {
                    if (i > 1) {
                        sb.append("; ");
                    }
                    String form = t.getForms().get(i);
                    if (VocabularyDeck.Format.HTML == format) {
                        form = VocabularyDeck.htmlEscape(form);
                    }
                    sb.append(form);
                }
                sb.append(")");
                if (VocabularyDeck.Format.HTML == format) {
                    sb.append("</span>");
                }
            }

            // Pronunciation
            if (null != t.getPronunciation()) {
                String p = t.getPronunciation();
                if (VocabularyDeck.Format.HTML == format) {
                    sb.append("<span class=\"answer-pronunciation\">");
                    p = VocabularyDeck.htmlEscape(p);
                }
                sb.append(" ").append(p);
                if (VocabularyDeck.Format.HTML == format) {
                    sb.append("</span>");
                }
            }

            sb.append(": ");

            // Type
            if (null != t.getType()) {
                String type = t.getType();
                if (VocabularyDeck.Format.HTML == format) {
                    sb.append("<span class=\"answer-type\">");
                    type = VocabularyDeck.htmlEscape(type);
                }
                sb.append(type).append(": ");
                if (VocabularyDeck.Format.HTML == format) {
                    sb.append("</span>");
                }
            }

            // Meaning
            String meaning = t.getMeaning();
            if (VocabularyDeck.Format.HTML == format) {
                sb.append("<span class=\"answer-meaning\">");
                meaning = VocabularyDeck.htmlEscape(meaning);
            }
            sb.append(meaning);
            if (VocabularyDeck.Format.HTML == format) {
                sb.append("</span>");
            }
            sb.append("\n");

            if (VocabularyDeck.Format.HTML == format) {
                sb.append("</div>\n");
            }
        }

        if (VocabularyDeck.Format.HTML == format) {
            sb.append("</div>\n");
        }

        return sb.toString();
    }

    @Override
    public String toString() {
        return definitions.get(0).getForms().get(0);
    }
}
