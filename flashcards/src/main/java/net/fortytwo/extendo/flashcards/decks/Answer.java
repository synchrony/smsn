package net.fortytwo.extendo.flashcards.decks;

import java.util.LinkedList;
import java.util.List;

/**
* User: josh
* Date: 4/12/11
* Time: 4:26 PM
*/
public class Answer {
    private final List<Form> forms = new LinkedList<Form>();
    private InformationSource source;
    private String meaning;
    private String type;
    private String pronunciation;

    public InformationSource getSource() {
        return source;
    }

    public List<Form> getForms() {
        return forms;
    }

    public String getMeaning() {
        return meaning;
    }

    public String getType() {
        return type;
    }

    public String getPronunciation() {
        return pronunciation;
    }

    public void setSource(final InformationSource source) {
        this.source = source;
    }

    public void addForm(final String label) {
        this.forms.add(new Form(label, null));
    }

    public void addForm(final String label,
                        final String url) {
        this.forms.add(new Form(label, url));
    }

    public void setMeaning(final String meaning) {
        this.meaning = meaning;
    }

    public void setType(final String type) {
        this.type = type;
    }

    public void setPronuncation(final String pronunciation) {
        this.pronunciation = pronunciation;
    }

    public class Form {
        private final String label;
        private final String url;

        public Form(final String label,
                    final String url) {
            this.label = label;
            this.url = url;
        }

        public String getLabel() {
            return label;
        }

        public String getUrl() {
            return url;
        }
    }
}
