package net.fortytwo.myotherbrain.flashcards.decks.vocab;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * A deck of Chinese characters with pronunciation and meaning.
 * <p/>
 * User: josh
 * Date: 3/9/11
 * Time: 6:04 PM
 */
public abstract class VocabularyDeck extends Deck<String, String> {
    public enum Format {TEXT, HTML}

    protected final Locale locale;
    private final Format format;
    private final Map<String, Card<String, String>> cards = new HashMap<String, Card<String, String>>();

    public VocabularyDeck(final String name,
                          final String label,
                          final Locale locale,
                          final Format format) throws IOException {
        super(name, label);

        this.locale = locale;
        this.format = format;

        Dictionary d = createVocabulary();
        for (String key : d.getKeys()) {
            List<Term> defs = d.getDefinitions(key);

            String n = Card.findCardName(key);
            cards.put(n, new LocalCard(n, this, defs));
        }
    }

    protected abstract Dictionary createVocabulary() throws IOException;

    @Override
    public Collection<Card<String, String>> getCards() {
        return cards.values();
    }

    public Card<String, String> getCard(final String name) {
        return cards.get(name);
    }

    private class LocalCard extends Card<String, String> {
        private final List<Term> defs;

        public LocalCard(final String name,
                         final Deck deck,
                         final List<Term> defs) {
            super(name, deck);

            this.defs = defs;
        }

        @Override
        public String getQuestion() {
            StringBuilder sb = new StringBuilder();

            if (Format.HTML == format) {
                sb.append("<div class=\"question\">\n");

                // Context
                sb.append("<div class=\"question-context\">").append(deck.getLabel()).append("</div>\n");
            }

            String question = defs.get(0).getForms().get(0) + " = ?";
            if (Format.HTML == format) {
                question = htmlEscape(question);
            }
            sb.append(question);

            if (Format.HTML == format) {
                sb.append("</div>\n");
            }

            return sb.toString();
        }

        @Override
        public String getAnswer() {
            StringBuilder sb = new StringBuilder();

            if (Format.HTML == format) {
                sb.append("<div class=\"answer\">\n");
            }

            for (Term t : defs) {
                // Source
                if (null != t.getSource()) {
                    String label = t.getSource().getLabel();
                    if (Format.HTML == format) {
                        sb.append("<div class=\"answer-source\">");
                        label = htmlEscape(label);
                    } else {
                        sb.append("[");
                    }

                    if (Format.HTML == format && null != t.getSource().getUrl()) {
                        sb.append("<a href=\"" + t.getSource().getUrl() + "\">");
                    }
                    sb.append(label);
                    if (Format.HTML == format) {
                        if (null != t.getSource().getUrl()) {
                            sb.append("</a>");
                        }
                    } else {
                        sb.append("]\n");
                    }

                    if (Format.HTML == format) {
                        sb.append("</div>\n");
                    }
                }

                if (Format.HTML == format) {
                    sb.append("<div class=\"answer-definition\">");
                }

                // Primary form;
                String primaryForm = t.getForms().get(0);
                if (Format.HTML == format) {
                    primaryForm = htmlEscape(primaryForm);
                    sb.append("<span class=\"answer-primary-form\">");
                }
                sb.append(primaryForm);
                if (Format.HTML == format) {
                    sb.append("</span>");
                }

                // Secondary forms
                if (2 <= t.getForms().size()) {
                    if (Format.HTML == format) {
                        sb.append("<span class=\"answer-secondary-forms\">");
                    }
                    sb.append(" (");
                    for (int i = 1; i < t.getForms().size(); i++) {
                        if (i > 1) {
                            sb.append("; ");
                        }
                        String form = t.getForms().get(i);
                        if (Format.HTML == format) {
                            form = htmlEscape(form);
                        }
                        sb.append(form);
                    }
                    sb.append(")");
                    if (Format.HTML == format) {
                        sb.append("</span>");
                    }
                }

                // Pronunciation
                if (null != t.getPronunciation()) {
                    String p = t.getPronunciation();
                    if (Format.HTML == format) {
                        sb.append("<span class=\"answer-pronunciation\">");
                        p = htmlEscape(p);
                    }
                    sb.append(" ").append(p);
                    if (Format.HTML == format) {
                        sb.append("</span>");
                    }
                }

                sb.append(": ");

                // Type
                if (null != t.getType()) {
                    String type = t.getType();
                    if (Format.HTML == format) {
                        sb.append("<span class=\"answer-type\">");
                        type = htmlEscape(type);
                    }
                    sb.append(type).append(": ");
                    if (Format.HTML == format) {
                        sb.append("</span>");
                    }
                }

                // Meaning
                String meaning = t.getMeaning();
                if (Format.HTML == format) {
                    sb.append("<span class=\"answer-meaning\">");
                    meaning = htmlEscape(meaning);
                }
                sb.append(meaning);
                if (Format.HTML == format) {
                    sb.append("</span>");
                }
                sb.append("\n");

                if (Format.HTML == format) {
                    sb.append("</div>\n");
                }
            }

            if (Format.HTML == format) {
                sb.append("</div>\n");
            }

            return sb.toString();
        }

        @Override
        public String toString() {
            return defs.get(0).getForms().get(0);
        }
    }

    public static String htmlEscape(final String s) {
        StringBuffer sb = new StringBuffer(s.length());
        // true if last char was blank
        boolean lastWasBlankChar = false;
        int len = s.length();
        char c;

        for (int i = 0; i < len; i++) {
            c = s.charAt(i);
            if (c == ' ') {
                // blank gets extra work,
                // this solves the problem you get if you replace all
                // blanks with &nbsp;, if you do that you loss
                // word breaking
                if (lastWasBlankChar) {
                    lastWasBlankChar = false;
                    sb.append("&nbsp;");
                } else {
                    lastWasBlankChar = true;
                    sb.append(' ');
                }
            } else {
                lastWasBlankChar = false;
                //
                // HTML Special Chars
                if (c == '"')
                    sb.append("&quot;");
                else if (c == '&')
                    sb.append("&amp;");
                else if (c == '<')
                    sb.append("&lt;");
                else if (c == '>')
                    sb.append("&gt;");
                else if (c == '\n')
                    // Handle Newline
                    sb.append("<br/>");
                else {
                    int ci = 0xffff & c;
                    if (ci < 160)
                        // nothing special only 7 Bit
                        sb.append(c);
                    else {
                        // Not 7 Bit use the unicode system
                        sb.append("&#");
                        sb.append(Integer.toString(ci));
                        sb.append(';');
                    }
                }
            }
        }

        //System.out.println("converted '" + s + "' to '" + sb.toString() + "'");
        return sb.toString();
    }
}
