package net.fortytwo.myotherbrain.flashcards.decks;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * User: josh
 * Date: 3/9/11
 * Time: 6:04 PM
 */
public class NPCRVocabulary extends Deck<String, String> {
    private final Map<String, Character> characters;
    private final Map<String, Card> cards = new HashMap<String, Card>();

    private class Character {
        String context1, context2;
        public String traditional;
        public String simplified;
        public String pinyin;
        public String meaning;
        public String type;
    }

    public NPCRVocabulary() throws IOException {
        super("npcr_vocabulary");

        characters = new HashMap<String, Character>();

        // New Practical Chinese Reader vocabulary list retrieved on 2011-3-17 from:
        //     http://hskflashcards.com/download.php
        InputStream is = NPCRVocabulary.class.getResourceAsStream("NPCR_vocabulary.txt");
        try {
            InputStreamReader r = new InputStreamReader(is, "UTF-8");
            BufferedReader br = new BufferedReader(r);
            String l;
            while ((l = br.readLine()) != null) {
                //System.out.println(l);
                Character c = new Character();
                c.context1 = l.trim();
                l = br.readLine();
                c.context2 = l.trim();
                l = br.readLine();
                c.traditional = l.trim();
                l = br.readLine();
                c.simplified = l.trim();
                l = br.readLine();
                c.pinyin = l.trim();
                l = br.readLine();
                c.meaning = l.trim();
                //System.out.println("\t" + c.meaning);
                l = br.readLine();
                c.type = l.trim();

                StringBuilder sb = new StringBuilder();
                for (int i = 0; i < c.simplified.length(); i++) {
                    sb.append("\\u").append(Integer.valueOf(c.simplified.charAt(i)));
                }
                Card card = new Card(sb.toString());

                characters.put(card.getName(), c);
                cards.put(card.getName(), card);
            }
        } finally {
            is.close();
        }
    }

    public String getQuestion(final Card card) {
        Character c = characters.get(card.getName());
        return c.simplified;
    }

    public String getAnswer(final Card card) {
        Character c = characters.get(card.getName());

        StringBuilder sb = new StringBuilder();

        sb.append(c.simplified)
                .append(" (").append(c.traditional).append(") ")
                .append(c.pinyin)
                .append(" -- ")
                .append(c.type)
                .append(": ")
                .append(c.meaning);

        return sb.toString();
    }

    @Override
    public Collection<Card> getCards() {
        return cards.values();
    }

    public Card getCard(final String name) {
        return cards.get(name);
    }
}
