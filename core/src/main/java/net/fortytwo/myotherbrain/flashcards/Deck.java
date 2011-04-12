package net.fortytwo.myotherbrain.flashcards;

import net.fortytwo.myotherbrain.flashcards.db.CloseableIterator;

/**
 * A set of cards with a common theme.
 * <p/>
 * User: josh
 * Date: 3/5/11
 * Time: 6:59 PM
 */
public abstract class Deck<Q, A> {
    private final String name;
    private final String label;

    public Deck(final String name,
                final String label) {
        this.name = name;
        this.label = label;
    }

    public String getName() {
        return name;
    }

    public String getLabel() {
        return label;
    }

    public abstract Card<Q, A> getCard(String name);

    public abstract CloseableIterator<Card<Q, A>> getCards();

    public enum Format {TEXT, HTML}

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
