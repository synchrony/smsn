package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.server.requests.FilteredResultsRequest;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * A service for removing isolated atoms (i.e. atoms with neither parents nor children) from an Extend-o-Brain graph
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RemoveIsolatedAtoms extends Action {

    @Override
    public String getName() {
        return "remove-isolated-atoms";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        FilteredResultsRequest r;
        r = new FilteredResultsRequest(request, p.user);

        p.filter = r.getFilter();
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        p.brain.getAtomGraph().removeIsolatedAtoms(p.filter);
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return true;
    }
}
