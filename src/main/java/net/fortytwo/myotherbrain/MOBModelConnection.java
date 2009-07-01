package net.fortytwo.myotherbrain;

import org.openrdf.elmo.ElmoManager;
import org.openrdf.elmo.ElmoManagerFactory;

import javax.xml.namespace.QName;

/**
 * Note: a connection is specific to a particular user's knowledge base.
 */
public class MOBModelConnection {
    private final ElmoManager elmoManager;

    public MOBModelConnection(final ElmoManagerFactory factory) {
        elmoManager = factory.createElmoManager();

        // Use an active transaction (rather than using auto-commit mode).
        // We will explicitly call commit() and rollback().
        elmoManager.getTransaction().begin();
    }

    public void commit() {
        elmoManager.getTransaction().commit();
    }

    public void rollback() {
        elmoManager.getTransaction().rollback();
    }

    public void close() {
        elmoManager.close();
    }

    public ElmoManager getElmoManager() {
        return elmoManager;
    }

    public <T> T create(final Class<T> c) {
        QName q = new QName(MyOtherBrain.randomURIString());
        return elmoManager.create(q, c);
    }
}
