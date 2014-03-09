package net.fortytwo.extendo.flashcards.decks.vocab;

import net.fortytwo.extendo.flashcards.decks.InformationSource;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.text.NumberFormat;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;

/**
 * The definition of a vocabulary term.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Term {//} implements Serializable {
    private static final String
            CONTEXT = "context",
            EXAMPLES = "examples",
            FORMS = "forms",
            MEANING = "meaning",
            PRONUNCIATION = "pronunciation",
            SOURCE = "source",
            TYPE = "type";

    private String type;
    private List<String> forms;
    private String pronunciation;
    private String meaning;
    private String context;
    private InformationSource source;
    private List<Term> examples;

    public Term() {
    }

    public Term(final JSONObject json) throws JSONException {
        type = json.optString(TYPE);
        pronunciation = json.optString(PRONUNCIATION);
        meaning = json.optString(MEANING);
        context = json.optString(CONTEXT);

        JSONObject s = json.optJSONObject(SOURCE);
        if (null != s) {
            source = new InformationSource(s);
        }

        JSONArray f = json.optJSONArray(FORMS);
        if (null != f && 1 <= f.length()) {
            forms = new LinkedList<String>();
            for (int i = 0; i < f.length(); i++) {
                forms.add(f.getString(i));
            }
        }

        JSONArray ex = json.optJSONArray(EXAMPLES);
        if (null != ex && 1 <= ex.length()) {
            examples = new LinkedList<Term>();
            for (int i = 0; i < ex.length(); i++) {
                examples.add(new Term(ex.getJSONObject(i)));
            }
        }
    }

    public JSONObject toJson() throws JSONException {
        JSONObject json = new JSONObject();

        if (null != type) {
            json.put(TYPE, type);
        }

        if (null != forms && 1 <= forms.size()) {
            JSONArray f = new JSONArray();
            for (String form : forms) {
                f.put(form);
            }
            json.put(FORMS, f);
        }

        if (null != pronunciation) {
            json.put(PRONUNCIATION, pronunciation);
        }

        if (null != meaning) {
            json.put(MEANING, meaning);
        }

        if (null != context) {
            json.put(CONTEXT, context);
        }

        if (null != source) {
            JSONObject s = source.toJson();
            json.put(SOURCE, s);
        }

        if (null != examples && 1 <= examples.size()) {
            JSONArray ex = new JSONArray();
            for (Term t : examples) {
                ex.put(t.toJson());
            }
            json.put(EXAMPLES, ex);
        }

        return json;
    }

    public String getType() {
        return type;
    }

    public void setType(final String type) {
        this.type = type;
    }

    public String normalizePrimaryForm(final Locale locale) {
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

    public InformationSource getSource() {
        return source;
    }

    public void setSource(InformationSource source) {
        this.source = source;
    }

    public static void main(final String[] args) {
        for (Locale l : NumberFormat.getAvailableLocales()) {
            System.out.println(l.getDisplayName() + " / " + l.getISO3Language());
        }
    }
}
