package net.fortytwo.myotherbrain.flashmob.model;

/**
 * Author: josh
 * Date: Jul 14, 2009
 * Time: 6:19:09 PM
 */
public class FlashMOBLiteral extends FlashMOBFirstClassItem {
    
    private String lexicalForm;
    private String datatypeURI;
    private String languageTag;

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
