package net.fortytwo.extendo.flashcards.decks;

import net.fortytwo.extendo.flashcards.Deck;

import java.util.LinkedList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class AnswerFormatter {
    private final Deck.Format format;
    private final List<Answer> answers;

    public AnswerFormatter(final Deck.Format format) {
        this.format = format;
        this.answers = new LinkedList<Answer>();
    }

    public void addAnswer(final Answer a) {
        answers.add(a);
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

        for (Answer a : answers) {
            // Source
            if (null != a.getSource()) {
                sb.append("[")
                        .append(a.getSource().getLabel())
                        .append("]\n");
            }

            if (a.getForms().size() > 0) {
                // Primary form;
                sb.append(a.getForms().get(0).getLabel());

                // Secondary forms
                if (2 <= a.getForms().size()) {
                    sb.append(" (");
                    for (int i = 1; i < a.getForms().size(); i++) {
                        if (i > 1) {
                            sb.append("; ");
                        }
                        sb.append(a.getForms().get(i).getLabel());
                    }
                    sb.append(")");
                }
            }

            // Pronunciation
            if (null != a.getPronunciation()) {
                String p = a.getPronunciation();
                sb.append(" ").append(p);
            }

            if ((0 < a.getForms().size() || null != a.getPronunciation())
                    && (null != a.getType() || null != a.getMeaning())) {
                sb.append(": ");
            }

            // Type
            if (null != a.getType()) {
                String type = a.getType();
                sb.append(type).append(": ");
            }

            // Meaning
            String meaning = a.getMeaning();
            if (null != meaning) {
                sb.append(meaning);
            }
            sb.append("\n");
        }

        return sb.toString();
    }

    private String outputHtml() {
        StringBuilder sb = new StringBuilder();

        sb.append("<div class=\"answer\">\n");

        for (Answer a : answers) {
            // Source
            if (null != a.getSource()) {
                String label = a.getSource().getLabel();
                sb.append("<div class=\"answer-source\">");
                label = Deck.htmlEscape(label);

                if (null != a.getSource().getUrl()) {
                    sb.append("<a href=\"").append(a.getSource().getUrl()).append("\">");
                }
                sb.append(label);
                if (null != a.getSource().getUrl()) {
                    sb.append("</a>");
                }

                sb.append("</div>\n");
            }

            sb.append("<div class=\"answer-definition\">");

            if (0 < a.getForms().size()) {
                // Primary form;
                Answer.Form primaryForm = a.getForms().get(0);
                sb.append("<span class=\"answer-primary-form\">");
                sb.append(formatForm(primaryForm));
                sb.append("</span>");

                // Secondary forms
                if (2 <= a.getForms().size()) {
                    sb.append("<span class=\"answer-secondary-forms\">");
                    sb.append(" (");
                    for (int i = 1; i < a.getForms().size(); i++) {
                        if (i > 1) {
                            sb.append("; ");
                        }
                        Answer.Form form = a.getForms().get(i);
                        sb.append(formatForm(form));
                    }
                    sb.append(")");
                    sb.append("</span>");
                }
            }

            // Pronunciation
            if (null != a.getPronunciation()) {
                String p = a.getPronunciation();
                sb.append("<span class=\"answer-pronunciation\">");
                p = Deck.htmlEscape(p);
                sb.append(" ").append(p);
                sb.append("</span>");
            }

            if ((0 < a.getForms().size() || null != a.getPronunciation())
                    && (null != a.getType() || null != a.getMeaning())) {
                sb.append(": ");
            }

            // Type
            if (null != a.getType()) {
                String type = a.getType();
                sb.append("<span class=\"answer-type\">");
                type = Deck.htmlEscape(type);
                sb.append(type).append(": ");
                sb.append("</span>");
            }

            // Meaning
            String meaning = a.getMeaning();
            if (null != meaning) {
                sb.append("<span class=\"answer-meaning\">");
                meaning = Deck.htmlEscape(meaning);
                sb.append(meaning);
                sb.append("</span>");
                sb.append("\n");
            }

            sb.append("</div>\n");
        }

        sb.append("</div>\n");

        return sb.toString();
    }

    private String formatForm(final Answer.Form form) {
        return null == form.getUrl()
                ? Deck.htmlEscape(form.getLabel())
                : "<a href=\"" + form.getUrl() + "\">" + Deck.htmlEscape(form.getLabel()) + "</a>";
    }
}
