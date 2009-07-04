package net.fortytwo.myotherbrain.writeapi;

import net.fortytwo.myotherbrain.model.MOBModelConnection;
import net.fortytwo.myotherbrain.model.MOB;
import net.fortytwo.myotherbrain.MyOtherBrain;
import org.openrdf.concepts.owl.Thing;

import javax.xml.namespace.QName;
import java.net.URI;
import java.util.Date;

/**
 * Author: josh
 * Date: Jul 3, 2009
 * Time: 7:11:55 PM
 */
public class WriteContext {
    private final MOBModelConnection connection;
    private final Quotas quotas;

    public WriteContext(final MOBModelConnection c) {
        connection = c;

        // TODO: use a real Quotas from the user's Session
        quotas = new Quotas();
    }

    public MOBModelConnection getConnection() {
        return connection;
    }

    public Quotas getQuotas() {
        return quotas;
    }

    public <T> T create(final Class<T> c) {
        QName q = new QName(MyOtherBrain.randomURIString());
        return create(q, c);
    }

    public <T> T create(final QName q, final Class<T> c) {
        return connection.getElmoManager().create(q, c);
    }

    public <T extends Thing> T designate(final QName qname,
                                         final Class<T> cl) {
        return connection.getElmoManager().designate(qname, cl);
    }

    public <T extends Thing> void removeDesignation(final T thing,
                                                    final Class<T> cl) {
        // Note: ignoring return value
        connection.getElmoManager().removeDesignation(thing, cl);
    }

    public <T extends Thing> void remove(final T thing) {
        connection.getElmoManager().remove(thing);
    }

    ////////////////////////////////////////////////////////////////////////////

    // Note: this particular check is by type rather than by field, all URIs
    // being equal.  A distinction is made between URIs for resources and
    // xsd:anyURI values in "reified" URIs.
    // For now, all valid java.net.URIs are assumed to be compatible with the
    // various components of the back-end.

    public URI normalizeResourceURI(final URI uri) throws QuotaException {
        quotas.checkResourceURI(uri);
        return uri;
    }

    public Date normalizeCreationTimeStamp(final Date creationTimeStamp) {
        // TODO: anything else to do?
        return creationTimeStamp;
    }

    public URI normalizeDatatypeURI(final URI datatypeURI) throws QuotaException {
        return normalizeResourceURI(datatypeURI);
    }

    public String normalizeDescription(final String description) throws QuotaException {
        String s = description.trim();
        quotas.checkDescription(s);
        return s;
    }

    public Float normalizeEmphasis(final Float emphasis) throws BadValueException {
        if (emphasis.isInfinite()
                || emphasis.isNaN()
                || emphasis < 0
                || emphasis > 1) {
            throw new BadValueException(MOB.EMPHASIS, emphasis);
        }

        return emphasis;
    }

    public String normalizeLanguageTag(final String languageTag) throws BadValueException {
        if (2 != languageTag.length()) {
            throw new BadValueException(MOB.LANGUAGETAG, languageTag);
        }

        // Note: currently, language tag values are not checked against any official list of language tags.

        return languageTag;
    }

    public Float normalizeLatitude(final Float latitude) throws BadValueException {
        if (latitude.isInfinite()
                || latitude.isNaN()
                || latitude < -90f
                || latitude > 90f) {
            throw new BadValueException(MOB.LATITUDE, latitude);
        }

        return latitude;
    }

    public String normalizeLexicalForm(final String lexicalForm) throws QuotaException {
        // Note: don't trim, as white space may be significant

        quotas.checkLexicalForm(lexicalForm);
        return lexicalForm;
    }

    public Float normalizeLongitude(final Float longitude) throws BadValueException {
        if (longitude.isInfinite()
                || longitude.isNaN()) {
            throw new BadValueException(MOB.LONGITUDE, longitude);
        }

        return longitude % 180f;
    }

    public String normalizeName(final String name) throws QuotaException {
        String s = name.trim();
        quotas.checkName(s);
        return s;
    }

    public String normalizeRepresentationMediaType(final String representationMediaType) throws QuotaException {
        String s = representationMediaType.trim();
        quotas.checkRepresentationMediaType(s);
        return s;
    }

    public String normalizeRepresentationSha1Sum(final String representationSha1Sum) throws BadValueException {
        if (40 != representationSha1Sum.length()) {
            throw new BadValueException(MOB.REPRESENTATIONSHA1SUM, representationSha1Sum);
        }

        for (byte b : representationSha1Sum.getBytes()) {
            if (!((b >= '0' && b <= '9') || (b >= 'a' && b <= 'f'))) {
                throw new BadValueException(MOB.REPRESENTATIONSHA1SUM, representationSha1Sum);
            }
        }

        return representationSha1Sum;
    }
}
