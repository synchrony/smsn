package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.server.action.requests.BasicViewRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;

/**
 * A service for finding root nodes of an Extend-o-Brain graph
 */
public class FindRoots extends Action {

    @Override
    public String getName() {
        return "find-roots";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        BasicViewRequest r;
        r = new BasicViewRequest(request, p.getUser());

        p.setHeight(r.getHeight());
        p.setStyleName(r.getStyleName());
        p.setFilter(r.getFilter());
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        Note n = p.getQueries().findRootAtoms(p.getFilter(), p.getStyle(), p.getHeight() - 1);
        try {
            addView(n, p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        p.getMap().put("title", "all roots");
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }
}
