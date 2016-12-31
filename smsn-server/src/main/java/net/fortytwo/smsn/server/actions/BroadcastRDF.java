package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.CoordinatorService;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.openrdf.rio.RDFFormat;

import javax.validation.constraints.NotNull;
import java.io.IOException;

/**
 * A service for broadcasting events modeled in RDF to all peers in the environment
 */
public class BroadcastRDF extends Action {

    @NotNull
    private String dataset;

    public String getDataset() {
        return dataset;
    }

    public void setDataset(String dataset) {
        this.dataset = dataset;
    }

    @Override
    public void parseRequest(final RequestParams p) throws IOException {
        p.setData(getDataset());
    }

    @Override
    protected void performTransaction(final RequestParams p) throws RequestProcessingException {
        // TODO: take RDF format as an input parameter
        RDFFormat format = RDFFormat.NTRIPLES;

        try {
            CoordinatorService.getInstance().pushUpdate(p.getData(), format);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    @Override
    protected boolean doesRead() {
        return false;
    }

    @Override
    protected boolean doesWrite() {
        // pushing of events is currently not considered writing... to the graph
        return false;
    }
}
