package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.undo.UndoableAction;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.Entity;

import javax.xml.namespace.QName;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.XMLGregorianCalendar;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashSet;
import java.util.Set;
import java.util.Date;
import java.util.GregorianCalendar;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public abstract class WriteAction extends UndoableAction<MOBModelConnection, NoSuchItemException> {
    // Note: the implementation of this class is assumed to be thread-safe.
    private static final DatatypeFactory DATATYPE_FACTORY;

    static {
        try {
            DATATYPE_FACTORY = DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    protected <T extends Thing> T toThing(final URI uri,
                                          final Class<T> cl,
                                          final MOBModelConnection c) throws NoSuchItemException {
        if (null == uri) {
            return null;
        } else {
            Entity entity = c.getElmoManager().find(toQName(uri));
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
                                                  final MOBModelConnection c) throws NoSuchItemException {
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