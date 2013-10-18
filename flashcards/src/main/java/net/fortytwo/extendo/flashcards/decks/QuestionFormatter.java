package net.fortytwo.extendo.flashcards.decks;

import net.fortytwo.extendo.flashcards.Deck;

/**
 * User: josh
 * Date: 4/12/11
 * Time: 3:59 PM
 */
public class QuestionFormatter {
    private final Deck deck;
    private final Deck.Format format;
    private String question;

    public QuestionFormatter(final Deck deck,
                             final Deck.Format format) {
        this.deck = deck;
        this.format = format;
    }

    public String getQuestion() {
        return question;
    }

    public void setQuestion(final String question) {
        this.question = question;
    }

    public String format() {
        switch (format) {
            case TEXT:
                return outputText();
            case HTML:
                return outputHtml();
            default:
                throw new IllegalStateException();
        }
    }

    private String outputText() {
        StringBuilder sb = new StringBuilder();

        sb.append(question);

        return sb.toString();
    }

    private String outputHtml() {
        StringBuilder sb = new StringBuilder();

        sb.append("<div class=\"question\">\n");

        // Context
        sb.append("<div class=\"question-context\">").append(deck.getLabel()).append("</div>\n");

        sb.append("<div class=\"question-body\">\n");
        String c = question.length() > 20
                ? "question-body-longtext"
                : "question-body-shorttext";
        sb.append("<span class=\"").append(c).append("\">");
        sb.append(Deck.htmlEscape(question));
        sb.append("</span>");
        sb.append("</div>\n");

        sb.append("</div>\n");

        return sb.toString();
    }
}

