package net.fortytwo.myotherbrain.flashmob.model;

/**
 * Author: josh
 * Date: Jul 14, 2009
 * Time: 6:19:01 PM
 */
public class AssociationBean extends FirstClassItemBean {
    
    private String associationSubject;
    private String associationObject;

    // Note: these will never constitute a cycle (at the Object level)
    private FirstClassItemBean subjectBean;
    private FirstClassItemBean objectBean;

    public String getAssociationSubject() {
        return associationSubject;
    }

    public void setAssociationSubject(final String associationSubject) {
        this.associationSubject = associationSubject;
    }

    public String getAssociationObject() {
        return associationObject;
    }

    public void setAssociationObject(final String associationObject) {
        this.associationObject = associationObject;
    }

    public FirstClassItemBean getSubjectBean() {
        return subjectBean;
    }

    public void setSubjectBean(FirstClassItemBean subjectBean) {
        this.subjectBean = subjectBean;
    }

    public FirstClassItemBean getObjectBean() {
        return objectBean;
    }

    public void setObjectBean(FirstClassItemBean objectBean) {
        this.objectBean = objectBean;
    }
}
