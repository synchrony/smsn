package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.server.requests.BasicViewRequest;
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
        r = new BasicViewRequest(request, p.user);

        p.height = r.getHeight();
        p.styleName = r.getStyleName();
        p.filter = r.getFilter();
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        Note n = p.queries.findRootAtoms(p.filter, p.style, p.height - 1);
        try {
            addView(n, p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        p.map.put("title", "all roots");
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }
}
