package net.fortytwo.myotherbrain.model.beans;

import net.fortytwo.myotherbrain.model.MOB;
import org.openrdf.elmo.annotations.rdf;

import java.net.URI;

/**
 * Author: josh
 * Date: May 7, 2009
 * Time: 7:52:19 PM
 */
@rdf(MOB.NAMESPACE + "Literal")
public interface Literal extends FirstClassItem {
    
    @rdf(MOB.NAMESPACE + "lexicalForm")
    String getLexicalForm();

    void setLexicalForm(String lexicalForm);

    @rdf(MOB.NAMESPACE + "languageTag")
    String getLanguageTag();

    void setLanguageTag(String languageTag);

    @rdf(MOB.NAMESPACE + "datatypeURI")
    URI getDatatypeURI();

    void setDatatypeURI(URI datatypeURI);
}
