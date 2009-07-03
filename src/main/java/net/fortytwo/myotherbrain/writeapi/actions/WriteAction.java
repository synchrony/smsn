package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.undo.UndoableAction;
import org.openrdf.elmo.Entity;

import javax.xml.namespace.QName;
import java.net.URI;
import java.net.URISyntaxException;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public abstract class WriteAction extends UndoableAction<MOBModelConnection, NoSuchItemException> {

    protected <T> T toEntity(final URI uri,
                             final Class<T> cl,
                             final MOBModelConnection c) throws NoSuchItemException {
        if (null == uri) {
            return null;
        } else {
            Entity entity = c.getElmoManager().find(new QName(uri.toString()));
            if (null == entity || !cl.isInstance(entity)) {
                throw new NoSuchItemException(uri, cl);
            }
            return (T) entity;
        }
    }

    protected URI toURI(final Entity entity) {
        if (null == entity) {
            return null;
        } else {
            QName q = entity.getQName();
            try {
                return new URI(q.getNamespaceURI() + q.getLocalPart());
            } catch (URISyntaxException e1) {
                throw new IllegalArgumentException("entity has bad QName: " + q);
            }
        }
    }
}