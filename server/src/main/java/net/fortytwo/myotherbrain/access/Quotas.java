package net.fortytwo.myotherbrain.access;

import net.fortytwo.myotherbrain.MyOtherBrain;
import net.fortytwo.myotherbrain.access.error.QuotaException;
import net.fortytwo.myotherbrain.util.properties.PropertyException;
import net.fortytwo.myotherbrain.util.properties.TypedProperties;

import java.net.URI;

/**
 * Author: josh
 * Date: May 8, 2009
 * Time: 3:46:49 AM
 */
public class Quotas {
    public static final int
            NAME_MAX_LENGTH,
            DESCRIPTION_MAX_LENGTH,
            RICHTEXTDESCRIPTION_MAX_LENGTH,
            LEXICALFORM_MAX_LENGTH,
            REPRESENTATIONMEDIATYPE_MAX_LENGTH,
            URI_MAX_LENGTH;

    static {
        TypedProperties props = MyOtherBrain.getConfiguration();
        String prefix = "net.fortytwo.myotherbrain.access.quotas.";
        try {
            NAME_MAX_LENGTH = props.getInt(prefix + "nameMaximumLength");
            DESCRIPTION_MAX_LENGTH = props.getInt(prefix + "descriptionMaximumLength");
            RICHTEXTDESCRIPTION_MAX_LENGTH = props.getInt(prefix + "richTextDescriptionMaximumLength");
            LEXICALFORM_MAX_LENGTH = props.getInt(prefix + "lexicalFormMaximumLength");
            REPRESENTATIONMEDIATYPE_MAX_LENGTH = props.getInt(prefix + "representationMediaTypeMaximumLength");
            URI_MAX_LENGTH = props.getInt(prefix + "uriMaximumLength");
        } catch (PropertyException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public void checkResourceURI(final URI uri) throws QuotaException {
        if (uri.toString().length() > URI_MAX_LENGTH) {
            throw new QuotaException("uri is too long");
        }
    }

    public void checkName(final String name) throws QuotaException {
        if (name.length() > NAME_MAX_LENGTH) {
            throw new QuotaException("name is too long");
        }
    }

    public void checkDescription(final String description) throws QuotaException {
        if (description.length() > DESCRIPTION_MAX_LENGTH) {
            throw new QuotaException("description is too long");
        }
    }

    public void checkRichTextDescription(final String richTextDescription) throws QuotaException {
        if (richTextDescription.length() > RICHTEXTDESCRIPTION_MAX_LENGTH) {
            throw new QuotaException("rich text description is too long");
        }
    }

    public void checkLexicalForm(final String lexicalForm) throws QuotaException {
        if (lexicalForm.length() > LEXICALFORM_MAX_LENGTH) {
            throw new QuotaException("lexical form is too long");
        }
    }

    public void checkRepresentationMediaType(final String representationMediaType) throws QuotaException {
        if (representationMediaType.length() > REPRESENTATIONMEDIATYPE_MAX_LENGTH) {
            throw new QuotaException("representation media type is too long");
        }
    }

    public void checkCreateItem() throws QuotaException {

    }
}
