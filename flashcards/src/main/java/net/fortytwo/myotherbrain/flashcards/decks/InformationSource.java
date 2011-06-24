package net.fortytwo.myotherbrain.flashcards.decks;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * User: josh
 * Date: 3/24/11
 * Time: 1:06 PM
 */
public class InformationSource {
    private static final String
            LABEL = "label",
            COMMENT = "comment",
            URL = "url",
            TIMESTAMP = "timestamp";

    private final String label;
    private String comment;
    private String url;
    private String timestamp;

    public InformationSource(final String label) {
        this.label = label;
    }

    public InformationSource(final JSONObject json) throws JSONException {
        label = json.getString(LABEL);
        comment = json.optString(COMMENT);
        url = json.optString(URL);
        timestamp = json.optString(TIMESTAMP);
    }

    public JSONObject toJson() throws JSONException {
        JSONObject json = new JSONObject();
        json.put(LABEL, label);
        if (null != comment) {
            json.put(COMMENT, comment);
        }
        if (null != url) {
            json.put(URL, url);
        }
        if (null != timestamp) {
            json.put(TIMESTAMP, timestamp);
        }
        return json;
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
