package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.server.action.requests.FilteredResultsRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

/**
 * A service for finding isolated atoms (i.e. atoms with no parents or children) in an Extend-o-Brain graph
 */
public class FindIsolatedAtoms extends Action {

    @Override
    public String getName() {
        return "find-isolated-atoms";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        FilteredResultsRequest r = new FilteredResultsRequest(request);

        p.setFilter(r.getFilter());

        SemanticSynchrony.logInfo("SmSn find-isolated-atoms");
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException {
        Note n = p.getQueries().findIsolatedAtoms(p.getFilter());
        try {
            addView(n, p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        p.getMap().put("title", "isolated atoms");
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }
}
