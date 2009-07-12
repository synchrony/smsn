package net.fortytwo.myotherbrain.flashmob.actions;

/**
 * Author: josh
 * Date: Jul 11, 2009
 * Time: 2:22:19 PM
 */
public class CreateWebResourceBean extends SubjectBean {
    protected String representationMediaType;
    protected String representationSha1Sum;

    public String getRepresentationMediaType() {
        return representationMediaType;
    }

    public void setRepresentationMediaType(final String representationMediaType) {
        this.representationMediaType = representationMediaType;
    }

    public String getRepresentationSha1Sum() {
        return representationSha1Sum;
    }

    public void setRepresentationSha1Sum(final String representationSha1Sum) {
        this.representationSha1Sum = representationSha1Sum;
    }
}
