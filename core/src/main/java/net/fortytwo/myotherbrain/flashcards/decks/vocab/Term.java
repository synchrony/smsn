package net.fortytwo.myotherbrain.flashcards.decks.vocab;

import java.text.NumberFormat;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;

/**
 * The definition of a vocabulary term.
 *
 * User: josh
 * Date: 3/23/11
 * Time: 7:26 PM
 */
public class Term {
    private final Locale locale;
    private String type;
    private List<String> forms;
    private String pronunciation;
    private String meaning;
    private String context;
    private VocabularySource source;
    private List<Term> examples;

    public Term(final Locale locale) {
        this.locale = locale;
    }

    public String getType() {
        return type;
    }

    public void setType(final String type) {
        this.type = type;
    }

    public String normalizePrimaryForm() {
        return forms.get(0).toLowerCase(locale);
    }

    public List<String> getForms() {
        return forms;
    }

    public void addForm(final String form) {
        if (null == forms) {
            forms = new LinkedList<String>();
        }

        forms.add(form);
    }

    public String getPronunciation() {
        return pronunciation;
    }

    public void setPronunciation(String pronunciation) {
        this.pronunciation = pronunciation;
    }

    public String getMeaning() {
        return meaning;
    }

    public void setMeaning(String meaning) {
        this.meaning = meaning;
    }

    public String getContext() {
        return context;
    }

    public void setContext(String context) {
        this.context = context;
    }

    public List<Term> getExamples() {
        return examples;
    }

    public void setExamples(List<Term> examples) {
        this.examples = examples;
    }

    public VocabularySource getSource() {
        return source;
    }

    public void setSource(VocabularySource source) {
        this.source = source;
    }

    public static void main(final String[] args) {
        for (Locale l : NumberFormat.getAvailableLocales()) {
            System.out.println(l.getDisplayName()+ " / " + l.getISO3Language());
        }
    }
}
