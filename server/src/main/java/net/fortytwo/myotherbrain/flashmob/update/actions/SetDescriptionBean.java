package net.fortytwo.myotherbrain.flashmob.update.actions;

/**
 * Author: josh
 * Date: Jul 11, 2009
 * Time: 2:26:24 PM
 */
public class SetDescriptionBean extends ResourceActionBean {
    private String description;
    private String richTextDescription;

    public String getDescription() {
        return description;
    }

    public void setDescription(final String description) {
        this.description = description;
    }

    public String getRichTextDescription() {
        return richTextDescription;
    }

    public void setRichTextDescription(String richTextDescription) {
        this.richTextDescription = richTextDescription;
    }
}
