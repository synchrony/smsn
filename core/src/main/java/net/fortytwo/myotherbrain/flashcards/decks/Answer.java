package net.fortytwo.myotherbrain.flashcards.decks;

import java.util.LinkedList;
import java.util.List;

/**
* User: josh
* Date: 4/12/11
* Time: 4:26 PM
*/
public class Answer {
    private final List<String> forms = new LinkedList<String>();
    private InformationSource source;
    private String meaning;
    private String type;
    private String pronunciation;

    public InformationSource getSource() {
        return source;
    }

    public List<String> getForms() {
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

    public void addForm(final String form) {
        this.forms.add(form);
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
}
