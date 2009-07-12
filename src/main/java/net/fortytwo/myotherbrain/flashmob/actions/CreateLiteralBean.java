package net.fortytwo.myotherbrain.flashmob.actions;

/**
 * Author: josh
 * Date: Jul 11, 2009
 * Time: 2:20:45 PM
 */
public class CreateLiteralBean extends CreateFirstClassItemBean {
    protected String lexicalForm;
    protected String datatypeURI;
    protected String languageTag;

    public String getLexicalForm() {
        return lexicalForm;
    }

    public void setLexicalForm(final String lexicalForm) {
        this.lexicalForm = lexicalForm;
    }

    public String getDatatypeURI() {
        return datatypeURI;
    }

    public void setDatatypeURI(final String datatypeURI) {
        this.datatypeURI = datatypeURI;
    }

    public String getLanguageTag() {
        return languageTag;
    }

    public void setLanguageTag(final String languageTag) {
        this.languageTag = languageTag;
    }
}
