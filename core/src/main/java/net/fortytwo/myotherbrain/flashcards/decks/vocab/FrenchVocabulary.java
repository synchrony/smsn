package net.fortytwo.myotherbrain.flashcards.decks.vocab;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

/**
 * User: josh
 * Date: 3/19/11
 * Time: 8:20 PM
 */
public class FrenchVocabulary extends VocabularyDeck {
    public FrenchVocabulary() throws IOException {
        super("french_vocabulary");
    }

    @Override
    public Map<String, Term> createVocabulary() throws IOException {
        Map<String, Term> terms = new HashMap<String, Term>();

        // French dictionary retrieved on 2011-3-22 from:
        //     http://www.dicts.info/uddl.php
        InputStream is = FrenchVocabulary.class.getResourceAsStream("OmegaWiki_French_dictionary.txt");
        try {
            InputStreamReader r = new InputStreamReader(is, "UTF-8");
            BufferedReader br = new BufferedReader(r);
            String l;
            while ((l = br.readLine()) != null) {
                l = l.trim();
                if (!l.startsWith("#")) {
                    Term t = new Term();
                    int tab = l.indexOf('\t');
                    String french = l.substring(0, tab).trim();
                    if (french.contains(";")) {
                        String[] a = french.split(";");
                        t.normativeForm = a[0].trim();
                        t.alternativeForms = new LinkedList<String>();
                        for (int i = 1; i < a.length; i++) {
                            t.alternativeForms.add(a[i].trim());
                        }
                    } else {
                        t.normativeForm = french;
                    }
                    t.meaning = l.substring(tab + 1).trim();
                    //System.out.println(c.normativeForm + ", " + c.pronunciation + ", " + c.meaning);

                    terms.put(findCardName(t), t);
                }
            }
        } finally {
            is.close();
        }

        return terms;
    }
}
