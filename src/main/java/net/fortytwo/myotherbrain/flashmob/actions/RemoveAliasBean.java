package net.fortytwo.myotherbrain.flashmob.actions;

/**
 * Author: josh
 * Date: Jul 11, 2009
 * Time: 2:23:35 PM
 */
public class RemoveAliasBean extends SubjectBean {
    protected String targetAlias;

    public String getTargetAlias() {
        return targetAlias;
    }

    public void setTargetAlias(final String targetAlias) {
        this.targetAlias = targetAlias;
    }
}
