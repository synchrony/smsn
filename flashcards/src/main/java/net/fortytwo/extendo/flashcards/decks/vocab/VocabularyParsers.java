package net.fortytwo.extendo.flashcards.decks.vocab;

import net.fortytwo.extendo.flashcards.decks.InformationSource;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

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
     * @param in     the <code>InputStream</code> to parse
     * @param dict   the dictionary into which to place parsed definitions
     * @param source the source of this definition (e.g. an online dictionary)
     * @throws IOException if parsing fails
     */
    public static void parseDictsInfoList(final InputStream in,
                                          final Dictionary dict,
                                          final InformationSource source) throws IOException {
        InputStreamReader r = new InputStreamReader(in, "UTF-8");
        BufferedReader br = new BufferedReader(r);
        String l;
        while ((l = br.readLine()) != null) {
            l = l.trim();
            if (!l.startsWith("#")) {
                Term t = new Term();
                if (null != source) {
                    t.setSource(source);
                }
                int tab = l.indexOf('\t');
                String lex = l.substring(0, tab).trim();
                if (lex.contains(";")) {
                    for (String s : lex.split(";")) {
                        t.addForm(simplify(s.trim()));
                    }
                } else {
                    t.addForm(simplify(lex));
                }
                t.setMeaning(l.substring(tab + 1).trim());
                //System.out.println(c.normativeForm + ", " + c.pronunciation + ", " + c.meaning);

                dict.add(t);
            }
        }
    }

    // Simplifies lexical forms from sources such as Wiktionary, which contain occasionally strange free-form text by users.
    private static String simplify(final String s) {
        String t = s.replaceAll("[\\x00-\\x1F]", " ");
        String tmp;
        do {
            tmp = t;
            t = t.replaceAll("  ", " ");
        } while (tmp.length() > t.length());
        return t;
    }

    public static void parseHSK4List(final InputStream is,
                                     final Dictionary dict,
                                     final InformationSource source) throws IOException {
        InputStreamReader r = new InputStreamReader(is, "UTF-8");
        BufferedReader br = new BufferedReader(r);
        String l;
        while ((l = br.readLine()) != null) {
            Term t = new Term();
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
                                     final InformationSource source) throws IOException {
        InputStreamReader r = new InputStreamReader(is, "UTF-8");
        BufferedReader br = new BufferedReader(r);
        String l;
        while ((l = br.readLine()) != null) {
            Term t = new Term();
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
