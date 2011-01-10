package net.fortytwo.myotherbrain.flashmob.model;

/**
 * Author: josh
 * Date: Jul 14, 2009
 * Time: 6:19:01 PM
 */
public class FlashMOBAssociation extends FlashMOBFirstClassItem {
    
    private String subjectURI;
    private String objectURI;

    // Note: these will never constitute a cycle (at the Object level)
    private FlashMOBFirstClassItem subject;
    private FlashMOBFirstClassItem object;

    public String getSubjectURI() {
        return subjectURI;
    }

    public void setSubjectURI(final String subjectURI) {
        this.subjectURI = subjectURI;
    }

    public String getObjectURI() {
        return objectURI;
    }

    public void setObjectURI(final String objectURI) {
        this.objectURI = objectURI;
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
