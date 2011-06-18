package net.fortytwo.myotherbrain.update;

import net.fortytwo.myotherbrain.update.undo.UndoableAction;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.Entity;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public abstract class WriteAction extends UndoableAction<WriteContext, UpdateException> {

    // Note: the implementation of this class is assumed to be thread-safe.
    private static final DatatypeFactory DATATYPE_FACTORY;

    static {
        try {
            DATATYPE_FACTORY = DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    protected final URI subject;

    protected WriteAction(final URI subject,
                          final WriteContext c) throws UpdateException {
        if (null == subject) {
            throw new NullPointerException();
        }

        this.subject = c.normalizeResourceURI(subject);
    }

    protected <T extends Thing> T toThing(final URI uri,
                                          final Class<T> cl,
                                          final WriteContext c) throws NoSuchItemException {
        if (null == uri) {
            return null;
        } else {
            Entity entity = c.getConnection().getElmoManager().find(toQName(uri));
            if (null == entity || !cl.isInstance(entity)) {
                throw new NoSuchItemException(uri, cl);
            }
            return (T) entity;
        }
    }

    protected URI toURI(final Thing thing) {
        return null == thing
                ? null
                : toURI(thing.getQName());
    }

    protected <T extends Thing> Set<T> toThingSet(final Set<URI> uris,
                                                  final Class<T> cl,
                                                  final WriteContext c) throws NoSuchItemException {
        Set<T> things = new HashSet<T>();

        for (URI u : uris) {
            T t = toThing(u, cl, c);
            things.add(t);
        }

        return things;
    }

    protected <T extends Thing> Set<URI> toURISet(final Set<T> things) {
        Set<URI> uris = new HashSet<URI>();

        for (T t : things) {
            URI u = toURI(t);
            if (null == u) {
                throw new NullPointerException();
            }

            uris.add(u);
        }

        return uris;
    }

    protected URI toURI(final QName qname) {
        try {
            return new URI(qname.getNamespaceURI() + qname.getLocalPart());
        } catch (URISyntaxException e1) {
            throw new IllegalArgumentException("QName can't be converted to a URI: " + qname);
        }
    }

    protected QName toQName(final URI uri) {
        return new QName(uri.toString());
    }


    protected XMLGregorianCalendar toXMLGregorianCalendar(final Date date) {
        GregorianCalendar cal = new GregorianCalendar();
        cal.setTime(date);
        return DATATYPE_FACTORY.newXMLGregorianCalendar(cal);
    }
}