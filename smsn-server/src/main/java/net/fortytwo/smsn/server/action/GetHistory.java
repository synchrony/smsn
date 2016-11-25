package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.server.requests.FilteredResultsRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.util.List;

/**
 * A service for finding recently visited atoms
 */
public class GetHistory extends Action {

    @Override
    public String getName() {
        return "history";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        FilteredResultsRequest r;
        r = new FilteredResultsRequest(request, p.getUser());

        p.setFilter(r.getFilter());
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        Iterable<Atom> atoms = getHistory(p.getBrain().getAtomGraph(), p.getFilter());

        try {
            addView(p.getQueries().customView(atoms, p.getFilter()), p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }
}
