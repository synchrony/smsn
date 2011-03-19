package net.fortytwo.myotherbrain.flashcards;

import net.fortytwo.myotherbrain.flashcards.db.CloseableIterator;
import net.fortytwo.myotherbrain.flashcards.db.GameHistory;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Set;

/**
 * User: josh
 * Date: 3/14/11
 * Time: 11:50 AM
 */
public abstract class Game<Q, A> {
    protected final Pile<Q, A> pile;
    protected final PriorityQueue<Card> active;
    protected final GameHistory history;

    public Game(final Pile<Q, A> pile,
                final GameHistory history) {
        this.history = history;
        this.pile = pile;

        // Create the active queue, which orders cards by increasing scheduled time.
        active = new PriorityQueue<Card>(1, new CardComparator());

        // Restore game history.
        Set<Card> cardsInHistory = new HashSet<Card>();
        CloseableIterator<Trial> h = history.getHistory();
        try {
            while (h.hasNext()) {
                Trial t = h.next();
                Card c = pile.drawCard(t.getDeckName(), t.getCardName());
                if (null != c) {
                    switch (t.getResult()) {
                        case Correct:
                            c.correct(t.getTime());
                            break;
                        case Incorrect:
                            c.incorrect(t.getTime());
                            break;
                        case Cancelled:
                            throw new IllegalStateException("the 'cancelled' result is not yet supported");
                    }
                    cardsInHistory.add(c);
                }
            }
        } finally {
            h.close();
        }

        for (Card c : cardsInHistory) {
            active.add(c);
        }
    }

    public abstract void play() throws GameplayException;

    public Card drawCard() {
        long now = System.currentTimeMillis();
        if (0 == active.size()) {
            if (pile.isEmpty()) {
                throw new IllegalStateException("empty card stack and no active cards");
            }

            return pile.drawRandomCard();
        } else if (active.peek().getNextTrial() <= now) {
            return active.poll();
        } else if (!pile.isEmpty()) {
            return pile.drawRandomCard();
        } else {
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

    public void replaceCard(final Card c) {
        active.add(c);
    }

    private static final long
            SECOND = 1000,
            MINUTE = SECOND * 60,
            HOUR = MINUTE * 60,
            DAY = HOUR * 24;

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

    protected void showQueue() {
        long now = System.currentTimeMillis();
        StringBuilder sb = new StringBuilder();
        sb.append("\t").append(active.size()).append(" cards:\n");
        sb.append("\t\t");

        List<Card> ordered = new LinkedList<Card>();
        ordered.addAll(active);
        Collections.sort(ordered, new CardComparator());

        boolean first = true;
        for (Card c : ordered) {
            if (first) {
                first = false;
            } else {
                sb.append(", ");
            }

            String d = formatDelay(c.getNextTrial() - now);
            sb.append(c.getName()).append(" (").append(d).append(")");
        }

        System.out.println(sb.toString());
    }

    private String pad(final long d) {
        return 0 == d ? "00" : 10 > d ? "0" + d : "" + d;
    }

    private class CardComparator implements Comparator<Card> {
        public int compare(final Card first, final Card second) {
            return ((Long) first.getNextTrial()).compareTo(second.getNextTrial());
        }
    }
}
