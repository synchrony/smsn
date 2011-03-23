package net.fortytwo.myotherbrain.flashcards;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;

/**
 * User: josh
 * Date: 3/17/11
 * Time: 5:08 PM
 */
public class PriorityPile<Q, A> implements Pile<Q, A> {
    private static final Random RANDOM = new Random();

    private final List<PileWithPriority> piles = new LinkedList<PileWithPriority>();
    private final Map<String, PileWithPriority> pilesByName = new HashMap<String, PileWithPriority>();

    private int sum = 0;

    public Card drawCard(final String deckName,
                         final String cardName) {
        PileWithPriority p = pilesByName.get(deckName);
        if (null == p) {
            return null;
        }

        boolean b = p.pile.isEmpty();
        Card c = p.pile.drawCard(deckName, cardName);
        if (p.pile.isEmpty() && !b) {
            piles.remove(p);
            sum -= p.priority;
        }

        return c;
    }

    public Card drawRandomCard() {
        if (0 == piles.size()) {
            throw new IllegalStateException("pile is empty (use pile.isEmpty() to avoid this error)");
        }

        double d = sum * RANDOM.nextDouble();
        //System.out.println("d = " + d);
        double s = 0;
        for (PileWithPriority p : piles) {
            s += p.priority;
            //System.out.println("\ts = " + s);
            if (s > d) {
                Card c = p.pile.drawRandomCard();
                if (p.pile.isEmpty()) {
                    piles.remove(p);
                    sum -= p.priority;
                }
                return c;
            }
        }

        throw new IllegalStateException();
    }

    public boolean isEmpty() {
        return 0 == piles.size();
    }

    /**
     * @param deck     a deck of flashcards_settings to add
     * @param priority a positive numeric value representing the priority of the deck (relatively high values indicating
     *                 relatively high priority).
     *                 New cards will drawn from a given deck with a probability proportional to the deck's priority.
     */
    public void addDeck(final Deck<Q, A> deck,
                        final int priority) {
        if (null == deck) {
            throw new NullPointerException();
        }

        if (1 > priority) {
            throw new IllegalArgumentException("priority must be a positive value");
        }

        PileWithPriority p = new PileWithPriority();
        p.pile = new SingleDeckPile<Q, A>(deck);

        if (p.pile.isEmpty()) {
            throw new IllegalArgumentException("deck must be non-empty");
        }

        p.priority = priority;
        piles.add(p);
        sum += priority;

        pilesByName.put(deck.getName(), p);
    }

    private class PileWithPriority {
        public Pile<Q, A> pile;
        public int priority;
    }
}
