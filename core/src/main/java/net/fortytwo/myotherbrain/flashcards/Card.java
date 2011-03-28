package net.fortytwo.myotherbrain.flashcards;

import java.util.Random;

/**
 * User: josh
 * Date: 3/5/11
 * Time: 7:01 PM
 */
public abstract class Card<Q, A> {
    private static final char[] HEX_CHARS = {
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
    };

    protected final String name;
    protected final Deck deck;

    public long lastTrial = 0;
    public long nextTrial = 0;

    /**
     * @param name a persistent, unique identifier, within the deck, for this card.
     * This identifier should not reveal the answer to the card, although it may be based on the question.
     * @param deck the deck to which this card belongs
     */
    public Card(final String name,
                final Deck deck) {
        this.name = name;
        this.deck = deck;
    }

    public abstract Q getQuestion();

    public abstract A getAnswer();

    public String getName() {
        return name;
    }

    public Deck getDeck() {
        return deck;
    }

    public long getNextTrial() {
        return nextTrial;
    }

    @Override
    public boolean equals(final Object other) {
        return other instanceof Card
                && ((Card) other).name.equals(name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public String toString() {
        return getName();
    }

    public static String findCardName(final String norm) {
        return unicodeEscape(norm);
    }

    // Note: escapes both high and low (whitespace < 0x20) characters.
    private static String unicodeEscape(final String s) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c < 32 || (c >> 7) > 0) {
                sb.append("\\u");
                sb.append(HEX_CHARS[(c >> 12) & 0xF]);
                sb.append(HEX_CHARS[(c >> 8) & 0xF]);
                sb.append(HEX_CHARS[(c >> 4) & 0xF]);
                sb.append(HEX_CHARS[c & 0xF]);
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }
}
