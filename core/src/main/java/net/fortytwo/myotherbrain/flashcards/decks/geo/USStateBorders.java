package net.fortytwo.myotherbrain.flashcards.decks.geo;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;
import net.fortytwo.myotherbrain.flashcards.db.CloseableIterator;
import net.fortytwo.myotherbrain.flashcards.db.TrivialCloseableIterator;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * User: josh
 * Date: 3/9/11
 * Time: 6:04 PM
 */
public class USStateBorders extends Deck<String, String> {
    private final Map<String, String[]> borders;
    private final Map<String, Card<String, String>> cards = new HashMap<String, Card<String, String>>();

    public USStateBorders() throws IOException {
        super("us_state_borders", "US state borders");

        borders = new HashMap<String, String[]>();

        // Matrix of US state borders retrieved from:
        //     http://www.statemaster.com/graph/geo_lan_bou_bor_cou-geography-land-borders
        InputStream is = USStateBorders.class.getResourceAsStream("US_state_borders.txt");
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(is));
            String l;
            while ((l = br.readLine()) != null) {
                l = l.trim();
                if (0 < l.length()) {
                    int i = l.indexOf(":");
                    String tail = l.substring(0, i).trim();
                    String[] h = l.substring(i + 1).trim().split(",");
                    String[] heads = new String[h.length];
                    borders.put(tail, heads);

                    for (int j = 0; j < h.length; j++) {
                        heads[j] = h[j].trim();
                    }

                    // Put the answer in alphabetical order.
                    Arrays.sort(heads);
                }
            }
        } finally {
            is.close();
        }

        // Add cards in random order.
        List<String> keys = new LinkedList<String>();
        keys.addAll(borders.keySet());
        Collections.shuffle(keys);
        //int count = 0;
        for (String s : keys) {
            Card<String, String> c = new LocalCard(s, this);
            cards.put(c.getName(), c);
            //if (++count > 5) break;
        }
    }

    @Override
    public CloseableIterator<Card<String, String>> getCards() {
        return new TrivialCloseableIterator<Card<String, String>>(cards.values().iterator());
    }

    public Card<String, String> getCard(final String name) {
        return cards.get(name);
    }

    private class LocalCard extends Card<String, String> {
        public LocalCard(final String name,
                         final Deck deck) {
            super(name, deck);
        }

        @Override
        public String getQuestion() {
            return "Which US states border on " + getName() + "?";
        }

        @Override
        public String getAnswer() {
            StringBuilder sb = new StringBuilder();
            String[] heads = borders.get(getName());
            for (int i = 0; i < heads.length; i++) {
                if (i > 0) {
                    sb.append((2 < heads.length) ? ", " : " ");
                    if (heads.length - 1 == i) {
                        sb.append("and ");
                    }
                }

                sb.append(heads[i]);
            }

            sb.append("\n");

            return sb.toString();
        }
    }
}
