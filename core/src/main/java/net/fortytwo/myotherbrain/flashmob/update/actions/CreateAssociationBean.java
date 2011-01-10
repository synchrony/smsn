package net.fortytwo.myotherbrain.flashmob.update.actions;

/**
 * Author: josh
 * Date: Jul 11, 2009
 * Time: 2:17:46 PM
 */
public class CreateAssociationBean extends CreateFirstClassItemBean {
    private String associationSubject;
    private String associationObject;

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
}
