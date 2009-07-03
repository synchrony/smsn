package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.model.beans.Literal;

import java.net.URI;
import java.util.Date;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class CreateLiteral extends CreateFirstClassItem {
    private final String lexicalForm;
    private final URI datatypeURI;
    private final String languageTag;

    public CreateLiteral(
            final URI subject,
            final String name,
            final String description,
            final URI icon,
            final URI sensitivity,
            final Float emphasis,
            final Date creationTimeStamp,
            final URI creationPlaceStamp,
            final String lexicalForm,
            final URI datatypeURI,
            final String languageTag) {
        super(subject, name, description, icon, sensitivity, emphasis,
                creationTimeStamp, creationPlaceStamp);

        if (null == lexicalForm) {
            throw new NullPointerException();
        }

        this.lexicalForm = lexicalForm;
        this.datatypeURI = datatypeURI;
        this.languageTag = languageTag;
    }

    protected void executeUndo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem subject = toThing(this.subject, FirstClassItem.class, c);
        c.getElmoManager().remove(subject);
    }

    protected void executeRedo(final MOBModelConnection c) throws NoSuchItemException {
        // TODO: is there any reason to use "designate" over "create"?
        Literal subject = c.getElmoManager().designate(toQName(this.subject), Literal.class);

        setCommonValues(subject, c);

        subject.setLexicalForm(lexicalForm);

        if (null != datatypeURI) {
            subject.setDatatypeURI(datatypeURI);
        }

        if (null != languageTag) {
            subject.setLanguageTag(languageTag);
        }
    }
}
