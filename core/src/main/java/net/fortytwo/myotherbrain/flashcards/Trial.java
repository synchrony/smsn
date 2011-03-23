package net.fortytwo.myotherbrain.flashcards;

import java.util.Date;

/**
 * User: josh
 * Date: 3/5/11
 * Time: 10:18 PM
 */
public class Trial {
    public enum Result {Correct, Incorrect, Cancelled}

    private final String deckName;
    private final String cardName;
    private final long time;
    private final Result result;

    public Trial(final String deckName,
                 final String cardName,
                 final long time,
                 final Result result) {
        this.deckName = deckName;
        this.cardName = cardName;
        this.time = time;
        this.result = result;
    }

    public String getDeckName() {
        return deckName;
    }

    public String getCardName() {
        return cardName;
    }

    public long getTime() {
        return time;
    }

    public Result getResult() {
        return result;
    }

    public String printPlainText() {
        StringBuilder sb = new StringBuilder();

        sb.append(deckName)
                .append(" ").append(cardName)
                .append(" ").append("(").append(new Date(time)).append(")")
                .append(" ").append(result);

        return sb.toString();
    }

    public String printTabDelimited() {
        StringBuilder sb = new StringBuilder();

        sb.append(deckName)
                .append("\t").append(cardName)
                .append("\t").append(time)
                .append("\t").append(result);

        return sb.toString();
    }
}
