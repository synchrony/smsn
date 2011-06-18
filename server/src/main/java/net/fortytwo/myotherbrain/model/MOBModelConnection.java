package net.fortytwo.myotherbrain.model;

import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.frames.FramesManager;
import net.fortytwo.myotherbrain.access.Quotas;
import org.openrdf.elmo.ElmoManager;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;

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

    public FramesManager getFramesManager() {
        // TODO
        return null;
    }

    public IndexableGraph getGraph() {
        // TODO
        return null;
    }

    public Quotas getQuotas() {
        return model.getQuotas();    
    }

    public SailConnection createSailConnection() throws SailException {
        // FIXME: where is this connection closed?
        return model.getSail().getConnection();
    }

    // FIXME: this is a hack
    /*
    public boolean freeTextSearchSupported() {
        return model.getSail() instanceof NeoSail;
    } */
}
