package net.fortytwo.extendo.flashcards;

import net.fortytwo.extendo.flashcards.db.CloseableIterator;
import net.fortytwo.extendo.flashcards.db.GameHistory;
import net.fortytwo.extendo.flashcards.decks.vocab.VocabularyDeck;

import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Random;
import java.util.Set;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class Game<Q, A> {
    private static final long
            SECOND = 1000,
            MINUTE = SECOND * 60,
            HOUR = MINUTE * 60,
            DAY = HOUR * 24;

    private long
            delayAfterFirstCorrect = 600000,
            delayAfterFirstIncorrect = 150000;

    // Randomized delays will be within this ratio of the precise value.
    private double delayImprecision = 0.1;

    private final Random random = new Random();

    protected final Pile<Q, A> pile;
    protected final PriorityQueue<Card<Q, A>> active;
    protected final GameHistory history;

    public Game(final Pile<Q, A> pile,
                final GameHistory history) {
        this.history = history;
        this.pile = pile;

        // Create the active queue, which orders cards by increasing scheduled time.
        active = new PriorityQueue<Card<Q, A>>(1, new CardComparator());

        // Restore game history.
        Set<Card<Q, A>> cardsInHistory = new HashSet<Card<Q, A>>();
        CloseableIterator<Trial> h = history.getHistory();
        try {
            while (h.hasNext()) {
                Trial t = h.next();
                Card<Q, A> c = pile.drawCard(t.getDeckName(), t.getCardName());
                if (null != c) {
                    switch (t.getResult()) {
                        case Correct:
                            correct(c, t.getTime());
                            break;
                        case Incorrect:
                            incorrect(c, t.getTime());
                            break;
                        case Cancelled:
                            throw new IllegalStateException("the '" + Trial.Result.Cancelled + "' result is not supported");
                    }
                    cardsInHistory.add(c);
                }
            }
        } finally {
            h.close();
        }

        for (Card<Q, A> c : cardsInHistory) {
            active.add(c);
        }
    }

    public abstract void play() throws GameplayException;

    private void correct(final Card c,
                         final long now) {
        long delay = 0 == c.lastTrial
                ? delayAfterFirstCorrect
                : increaseDelay(now - c.lastTrial);
        delay = randomizeDelay(delay);
        c.nextTrial = now + delay;
        c.lastTrial = now;
    }

    private void incorrect(final Card c,
                           final long now) {
        c.lastTrial = now;
        long delay = randomizeDelay(delayAfterFirstIncorrect);
        c.nextTrial = c.lastTrial + delay;
    }

    protected void logAndReplace(final Card<Q, A> c,
                                 final Trial.Result result) throws GameplayException {
        long now = System.currentTimeMillis();

        switch (result) {
            case Correct:
                correct(c, now);
                break;
            case Incorrect:
                incorrect(c, now);
                break;
            case Cancelled:
                throw new IllegalStateException("the '" + Trial.Result.Cancelled + "' result is not supported");
        }

        try {
            history.log(new Trial(c.getDeck().getName(), c.getName(), now, result));
        } catch (IOException e) {
            throw new GameplayException(e);
        }

        replaceCard(c);
    }

    private long increaseDelay(final long delay) {
        return delay * 2;
    }

    private long randomizeDelay(final long delay) {
        long d = (long) (delayImprecision * delay * (random.nextDouble() * 2 - 1));
        return delay + d;
    }

    public Card<Q, A> drawCard() {
        long now = System.currentTimeMillis();

        // No active cards; draw from the pile.
        if (0 == active.size()) {
            if (pile.isEmpty()) {
                throw new IllegalStateException("empty card stack and no active cards");
            }

            return pile.drawRandomCard();
        }

        // There is an overdue active card; draw it.
        else if (active.peek().getNextTrial() <= now) {
            return active.poll();
        }

        // There are no overdue cards, but the pile is not empty; draw from the pile.
        else if (!pile.isEmpty()) {
            return pile.drawRandomCard();
        }

        // There are no overdue cards and the pile is empty; wait until a card becomes overdue.
        else {
            long delay = active.peek().getNextTrial() - now;
            System.out.println("waiting " + delay + "ms");
            synchronized (this) {
                try {
                    wait(delay);
                } catch (InterruptedException e) {
                    e.printStackTrace(System.err);
                    System.exit(1);
                }
            }
            return active.poll();
        }
    }

    public void replaceCard(final Card<Q, A> c) {
        active.add(c);
    }

    protected String formatDelay(final long delay) {
        StringBuilder sb = new StringBuilder();

        long rem = delay;
        if (rem < 0) {
            sb.append("-");
            rem = -rem;
        }

        long day = rem / DAY;
        rem -= day * DAY;
        long hour = rem / HOUR;
        rem -= hour * HOUR;
        long minute = rem / MINUTE;
        rem -= minute * MINUTE;
        long second = rem / SECOND;

        if (0 < day) {
            sb.append(day).append("d,");
        }

        if (0 < hour) {
            sb.append(hour).append(":").append(pad(minute)).append(":").append(pad(second)).append("h");
        } else if (0 < minute) {
            sb.append(minute).append(":").append(pad(second)).append("m");
        } else if (0 < second) {
            sb.append(second).append("s");
        } else {
            sb.append(rem).append("ms");
        }

        return sb.toString();
    }

    public String showQueue(final VocabularyDeck.Format format) {
        long now = System.currentTimeMillis();

        int overdue = 0;
        for (Card c : active) {
            if (c.getNextTrial() <= now) {
                overdue++;
            }
        }
        int percentOverdue = (overdue * 100) / active.size();

        StringBuilder sb = new StringBuilder();

        if (Deck.Format.HTML == format) {
            sb.append("<div>");
            sb.append(active.size()).append(" cards<br/>\n");
            sb.append(overdue).append(" (").append(percentOverdue).append("%) due for review");
            sb.append("</div>\n<br/>\n<div>\n");
        } else {
            sb.append("\t").append(active.size()).append(" cards\n");
            sb.append(overdue).append(" (").append(percentOverdue).append("%) due for review\n");
            sb.append("\t\t");
        }

        List<Card> ordered = new LinkedList<Card>();
        ordered.addAll(active);
        Collections.sort(ordered, new CardComparator());

        if (Deck.Format.HTML == format) {
            sb.append("<span class=\"background\">");
        }

        boolean first = true;
        for (Card c : ordered) {
            if (first) {
                first = false;
            } else {
                sb.append(", ");
            }

            if (Deck.Format.HTML == format) {
                sb.append("<span class=\"foreground\">");
                sb.append(Deck.htmlEscape(c.toString()));
                sb.append("</span>");
            } else {
                sb.append(c);
            }

            String d = formatDelay(c.getNextTrial() - now);
            sb.append(" (").append(d).append(")");
        }

        if (Deck.Format.HTML == format) {
            sb.append("</span>\n</div>\n");
        }

        return sb.toString();
    }

    private String pad(final long d) {
        return 0 == d ? "00" : 10 > d ? "0" + d : "" + d;
    }

    private class CardComparator implements Comparator<Card> {
        public int compare(final Card first, final Card second) {
            return ((Long) first.getNextTrial()).compareTo(second.getNextTrial());
        }
    }

    public void setDelayAfterFirstCorrect(long delay) {
        this.delayAfterFirstCorrect = delay;
    }

    public void setDelayAfterFirstIncorrect(long delay) {
        this.delayAfterFirstIncorrect = delay;
    }

    public void setDelayImprecision(double imprecision) {
        this.delayImprecision = imprecision;
    }

}
