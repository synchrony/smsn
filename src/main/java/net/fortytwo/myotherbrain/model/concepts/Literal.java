package net.fortytwo.myotherbrain.model.concepts;

import net.fortytwo.myotherbrain.model.MOB;
import org.openrdf.elmo.annotations.rdf;

import java.net.URI;

/**
 * Author: josh
 * Date: May 7, 2009
 * Time: 7:52:19 PM
 */
@rdf(MOB.LITERAL)
public interface Literal extends FirstClassItem {

    @rdf(MOB.DATATYPEURI)
    URI getDatatypeURI();

    void setDatatypeURI(URI datatypeURI);

    @rdf(MOB.LANGUAGETAG)
    String getLanguageTag();

    void setLanguageTag(String languageTag);

    @rdf(MOB.LEXICALFORM)
    String getLexicalForm();

    void setLexicalForm(String lexicalForm);
}
