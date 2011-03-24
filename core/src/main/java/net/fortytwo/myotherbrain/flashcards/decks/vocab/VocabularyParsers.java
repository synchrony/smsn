package net.fortytwo.myotherbrain.flashcards.decks.vocab;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Locale;

/**
 * A number of static convenience methods for parsing vocabulary lists in different formats.
 * <p/>
 * User: josh
 * Date: 3/23/11
 * Time: 7:24 PM
 */
public class VocabularyParsers {
    /**
     * Parses vocabulary lists in the format provided by http://www.dicts.info/uddl.php
     *
     * @param is     the <code>InputStream</code> to parse
     * @param dict   the dictionary into which to place parsed definitions
     * @param locale the language of this term
     * @param source the source of this definition (e.g. an online dictionary)
     * @throws IOException if parsing fails
     */
    public static void parseDictsInfoList(final InputStream is,
                                          final Dictionary dict,
                                          final Locale locale,
                                          final VocabularySource source) throws IOException {
        InputStreamReader r = new InputStreamReader(is, "UTF-8");
        BufferedReader br = new BufferedReader(r);
        String l;
        while ((l = br.readLine()) != null) {
            l = l.trim();
            if (!l.startsWith("#")) {
                Term t = new Term(locale);
                if (null != source) {
                    t.setSource(source);
                }
                int tab = l.indexOf('\t');
                String french = l.substring(0, tab).trim();
                if (french.contains(";")) {
                    String[] a = french.split(";");
                    t.addForm(a[0].trim());
                    for (int i = 1; i < a.length; i++) {
                        t.addForm(a[i].trim());
                    }
                } else {
                    t.addForm(french);
                }
                t.setMeaning(l.substring(tab + 1).trim());
                //System.out.println(c.normativeForm + ", " + c.pronunciation + ", " + c.meaning);

                dict.add(t);
            }
        }
    }

    public static void parseHSK4List(final InputStream is,
                                     final Dictionary dict,
                                     final VocabularySource source) throws IOException {
        InputStreamReader r = new InputStreamReader(is, "UTF-8");
        BufferedReader br = new BufferedReader(r);
        String l;
        while ((l = br.readLine()) != null) {
            Term t = new Term(Locale.CHINESE);
            if (null != source) {
                t.setSource(source);
            }

            int tab = l.indexOf('\t');
            int star = l.indexOf('*');
            t.addForm(l.substring(0, tab).trim());
            t.setPronunciation(l.substring(tab + 1, star).trim());
            t.setMeaning(l.substring(star + 1).trim());

            dict.add(t);
        }
    }

    public static void parseNPCRList(final InputStream is,
                                     final Dictionary dict,
                                     final VocabularySource source) throws IOException {
        InputStreamReader r = new InputStreamReader(is, "UTF-8");
        BufferedReader br = new BufferedReader(r);
        String l;
        while ((l = br.readLine()) != null) {
            Term t = new Term(Locale.CHINESE);
            if (null != source) {
                t.setSource(source);
            }

            String context1, context2;
            context1 = l.trim();
            l = br.readLine();
            context2 = l.trim();
            t.setContext(context1 + ":" + context2);
            l = br.readLine();
            String trad = l.trim();
            l = br.readLine();
            t.addForm(l.trim());
            t.addForm(trad);
            l = br.readLine();
            t.setPronunciation(l.trim());
            l = br.readLine();
            t.setMeaning(l.trim());
            l = br.readLine();
            t.setType(l.trim());

            dict.add(t);
        }
    }
}
