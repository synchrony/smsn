package net.fortytwo.myotherbrain.model.concepts;

import net.fortytwo.myotherbrain.model.MOBOntology;
import org.openrdf.elmo.annotations.rdf;

import java.net.URI;

/**
 * Author: josh
 * Date: May 7, 2009
 * Time: 7:52:19 PM
 */
@rdf(MOBOntology.LITERAL)
public interface Literal extends Atom {

    @rdf(MOBOntology.DATATYPEURI)
    URI getDatatypeURI();

    void setDatatypeURI(URI datatypeURI);

    @rdf(MOBOntology.LANGUAGETAG)
    String getLanguageTag();

    void setLanguageTag(String languageTag);

    @rdf(MOBOntology.LEXICALFORM)
    String getLexicalForm();

    void setLexicalForm(String lexicalForm);
}
