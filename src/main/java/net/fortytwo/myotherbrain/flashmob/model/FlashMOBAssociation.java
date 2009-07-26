package net.fortytwo.myotherbrain.flashmob.model;

/**
 * Author: josh
 * Date: Jul 14, 2009
 * Time: 6:19:01 PM
 */
public class FlashMOBAssociation extends FlashMOBFirstClassItem {
    
    private String subjectUri;
    private String objectUri;

    // Note: these will never constitute a cycle (at the Object level)
    private FlashMOBFirstClassItem subject;
    private FlashMOBFirstClassItem object;

    public String getSubjectUri() {
        return subjectUri;
    }

    public void setSubjectUri(final String subjectUri) {
        this.subjectUri = subjectUri;
    }

    public String getObjectUri() {
        return objectUri;
    }

    public void setObjectUri(final String objectUri) {
        this.objectUri = objectUri;
    }

    public FlashMOBFirstClassItem getSubject() {
        return subject;
    }

    public void setSubject(FlashMOBFirstClassItem subject) {
        this.subject = subject;
    }

    public FlashMOBFirstClassItem getObject() {
        return object;
    }

    public void setObject(FlashMOBFirstClassItem object) {
        this.object = object;
    }
}
