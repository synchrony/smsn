package net.fortytwo.extendo.flashcards.decks.vocab;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * User: josh
 * Date: 3/24/11
 * Time: 1:16 PM
 */
public class Dictionary {
    private final Map<String, List<Term>> terms;
    private final Locale locale;

    public Dictionary(final Locale locale) {
        this.locale = locale;
        terms = new HashMap<String, List<Term>>();
    }

    public void add(final Term t) {
        String s = t.normalizePrimaryForm(locale);

        List<Term> defs = terms.get(s);
        if (null == defs) {
            defs = new LinkedList<Term>();
            terms.put(s, defs);
        }

        defs.add(t);
    }

    public Set<String> getKeys() {
        return terms.keySet();
    }

    public List<Term> getDefinitions(final String key) {
        return terms.get(key);
    }
}
