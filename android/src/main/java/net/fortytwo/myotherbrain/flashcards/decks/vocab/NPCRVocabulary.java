package net.fortytwo.myotherbrain.flashcards.decks.vocab;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

/**
 * User: josh
 * Date: 3/9/11
 * Time: 6:04 PM
 */
public class NPCRVocabulary extends VocabularyDeck {

    public NPCRVocabulary() throws IOException {
        super("npcr_vocabulary");
    }

    @Override
    public Map<String, Term> createVocabulary() throws IOException {
        Map<String, Term> characters = new HashMap<String, Term>();

        // New Practical Chinese Reader vocabulary list retrieved on 2011-3-17 from:
        //     http://hskflashcards.com/download.php
        InputStream is = NPCRVocabulary.class.getResourceAsStream("NPCR_vocabulary.txt");
        try {
            InputStreamReader r = new InputStreamReader(is, "UTF-8");
            BufferedReader br = new BufferedReader(r);
            String l;
            while ((l = br.readLine()) != null) {
                //System.out.println(l);
                Term c = new Term();
                String context1, context2;
                context1 = l.trim();
                l = br.readLine();
                context2 = l.trim();
                c.context = context1 + ":" + context2;
                l = br.readLine();
                c.alternativeForm = l.trim();
                l = br.readLine();
                c.normativeForm = l.trim();
                l = br.readLine();
                c.pronunciation = l.trim();
                l = br.readLine();
                c.meaning = l.trim();
                //System.out.println("\t" + c.meaning);
                l = br.readLine();
                c.type = l.trim();

                StringBuilder sb = new StringBuilder();
                for (int i = 0; i < c.normativeForm.length(); i++) {
                    sb.append("\\u").append(Integer.valueOf(c.normativeForm.charAt(i)));
                }

                characters.put(sb.toString(), c);
            }
        } finally {
            is.close();
        }

        return characters;
    }
}
