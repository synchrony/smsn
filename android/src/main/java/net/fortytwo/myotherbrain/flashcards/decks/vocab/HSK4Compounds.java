package net.fortytwo.myotherbrain.flashcards.decks.vocab;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

/**
 * User: josh
 * Date: 3/19/11
 * Time: 8:20 PM
 */
public class HSK4Compounds extends VocabularyDeck {

    public HSK4Compounds() throws IOException {
        super("hsk4_compounds");
    }

    @Override
    public Map<String, Term> createVocabulary() throws IOException {
        Map<String, Term> terms = new HashMap<String, Term>();

        // HSK level 4 vocabulary list retrieved on 2011-3-19 from:
        //     http://www.chinese-forums.com/index.php?/topic/14829-hsk-character-lists/
        // (provided by user renzhe)
        InputStream is = HSK4Characters.class.getResourceAsStream("hsk4-multitab.txt");
        try {
            InputStreamReader r = new InputStreamReader(is, "UTF-8");
            BufferedReader br = new BufferedReader(r);
            String l;
            while ((l = br.readLine()) != null) {
                Term c = new Term();
                int tab = l.indexOf('\t');
                int star = l.indexOf('*');
                c.normativeForm = l.substring(0, tab).trim();
                c.pronunciation = l.substring(tab + 1, star).trim();
                c.meaning = l.substring(star + 1).trim();
                //System.out.println(c.normativeForm + ", " + c.pronunciation + ", " + c.meaning);

                StringBuilder sb = new StringBuilder();
                for (int i = 0; i < c.normativeForm.length(); i++) {
                    sb.append("\\u").append(Integer.valueOf(c.normativeForm.charAt(i)));
                }

                terms.put(sb.toString(), c);
            }
        } finally {
            is.close();
        }

        return terms;
    }
}
