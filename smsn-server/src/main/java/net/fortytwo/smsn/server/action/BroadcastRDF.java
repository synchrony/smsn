package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.CoordinatorService;
import net.fortytwo.smsn.server.Request;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.util.TypedProperties;
import org.json.JSONException;
import org.json.JSONObject;
import org.openrdf.rio.RDFFormat;
import org.openrdf.sail.SailException;

import java.io.IOException;
import java.security.Principal;

/**
 * A service for broadcasting events modeled in RDF to all peers in the environment
 */
public class BroadcastRDF extends Action {

    private final CoordinatorService coordinator;

    public BroadcastRDF() {

        coordinator = CoordinatorService.getInstance();
    }

    @Override
    public String getName() {
        return "broadcast-rdf";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {
        BroadcastRdfRequest r;
        r = new BroadcastRdfRequest(request, p.user);

        p.data = r.dataset;

        SemanticSynchrony.logInfo("smsn broadcast-rdf");
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException {
        // TODO: take RDF format as an input parameter
        RDFFormat format = RDFFormat.NTRIPLES;

        try {
            coordinator.pushUpdate(p.data, format);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    protected boolean doesRead() {
        return false;
    }

    protected boolean doesWrite() {
        // pushing of events is currently not considered writing... to the graph
        return false;
    }

    protected class BroadcastRdfRequest extends Request {
        public final String dataset;

        public BroadcastRdfRequest(JSONObject json, Principal user) throws JSONException {
            super(json, user);

            dataset = this.json.getString(Params.DATASET);
        }
    }
}
