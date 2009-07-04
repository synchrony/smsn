package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.model.beans.Literal;
import net.fortytwo.myotherbrain.writeapi.WriteContext;
import net.fortytwo.myotherbrain.writeapi.WriteException;

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
            URI subject,
            String name,
            String description,
            URI icon,
            URI sensitivity,
            Float emphasis,
            Date creationTimeStamp,
            URI creationPlaceStamp,
            String lexicalForm,
            URI datatypeURI,
            String languageTag,
            final WriteContext c) throws WriteException {
        super(subject, name, description, icon, sensitivity, emphasis,
                creationTimeStamp, creationPlaceStamp, c);

        if (null == lexicalForm) {
            throw new NullPointerException();
        } else {
            lexicalForm = c.normalizeLexicalForm(lexicalForm);
        }

        if (null != datatypeURI) {
            datatypeURI = c.normalizeDatatypeURI(datatypeURI);
        }

        if (null != languageTag) {
            languageTag = c.normalizeLanguageTag(languageTag);
        }

        this.lexicalForm = lexicalForm;
        this.datatypeURI = datatypeURI;
        this.languageTag = languageTag;
    }

    @Override
    protected void executeUndo(final WriteContext c) throws WriteException {
        FirstClassItem subject = toThing(this.subject, FirstClassItem.class, c);
        c.remove(subject);
    }

    @Override
    protected void executeRedo(final WriteContext c) throws WriteException {
        // TODO: is there any reason to use "designate" over "create"?
        Literal subject = c.designate(toQName(this.subject), Literal.class);

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
