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
    protected final List<Card> pool;
    protected final PriorityQueue<Card> active;
    protected final Deck<Q, A> deck;
    protected final GameHistory history;

    public Game(final Deck<Q, A> deck,
                final GameHistory history) {
        this.deck = deck;
        this.history = history;

        // Create the pool of all cards.
        pool = new LinkedList<Card>();
        pool.addAll(deck.getCards());

        // Randomize the pool, so that every cold start is not the same.
        Collections.shuffle(pool);

        // Create the active queue, which orders cards by increasing scheduled time.
        active = new PriorityQueue<Card>(1, new CardComparator());

        // Restore game history.
        Set<Card> cardsInHistory = new HashSet<Card>();
        CloseableIterator<Trial> h = history.getHistory(deck);
        try {
            while (h.hasNext()) {
                Trial t = h.next();
                Card c = deck.getCard(t.getCardName());
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
            pool.remove(c);
            active.add(c);
        }
    }

    public abstract void play() throws GameplayException;

    public Card drawCard() {
        long now = System.currentTimeMillis();
        if (0 == active.size()) {
            if (0 == pool.size()) {
                throw new IllegalStateException("empty card pool");
            }

            return pool.remove(0);
        } else if (active.peek().getNextTrial() <= now) {
            return active.poll();
        } else if (0 < pool.size()) {
            return pool.remove(0);
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
        StringBuilder sb = new StringBuilder("\t\t");

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
