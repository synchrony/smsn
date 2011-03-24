package net.fortytwo.myotherbrain.flashcards.decks.vocab;

/**
* User: josh
* Date: 3/24/11
* Time: 1:06 PM
*/
public class VocabularySource {
    private final String label;
    private String comment;
    private String url;
    private String timestamp;

    public VocabularySource(final String label) {
        this.label = label;
    }

    public String getLabel() {
        return label;
    }

    public String getComment() {
        return comment;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(final String url) {
        this.url = url;
    }

    public String getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(String timestamp) {
        this.timestamp = timestamp;
    }
}
