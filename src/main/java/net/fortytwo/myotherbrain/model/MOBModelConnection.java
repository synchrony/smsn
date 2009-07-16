package net.fortytwo.myotherbrain.model;

import net.fortytwo.myotherbrain.update.Quotas;
import org.openrdf.elmo.ElmoManager;

/**
 * Note: a connection is specific to a particular user's knowledge base.
 */
public class MOBModelConnection {
    private final MOBModel model;
    private final ElmoManager elmoManager;

    public MOBModelConnection(final MOBModel model) {
        this.model = model;
        elmoManager = model.getElmoManagerFactory().createElmoManager();

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

    public Quotas getQuotas() {
        return model.getQuotas();    
    }
}
