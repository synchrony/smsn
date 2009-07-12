package net.fortytwo.myotherbrain.flashmob.actions;

/**
 * Author: josh
 * Date: Jul 11, 2009
 * Time: 2:24:09 PM
 */
public class RemoveMarkerTagBean extends SubjectBean {
    protected String targetMarkerTag;

    public String getTargetMarkerTag() {
        return targetMarkerTag;
    }

    public void setTargetMarkerTag(final String targetMarkerTag) {
        this.targetMarkerTag = targetMarkerTag;
    }
}
