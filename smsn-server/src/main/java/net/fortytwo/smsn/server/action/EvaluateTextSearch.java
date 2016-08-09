package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.server.requests.BasicSearchRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.security.Principal;

/**
 * A service for executing keyword search over an Extend-o-Brain graph
 */
public class EvaluateTextSearch extends Action {

    private static final int DEFAULT_VALUE_LENGTH_CUTOFF = 100;

    @Override
    public String getName() {
        return "search";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {

        SearchRequest r = new SearchRequest(request, p.user);

        p.height = r.getHeight();
        p.queryType = r.getQueryType();
        p.query = r.getQuery();
        p.styleName = r.getStyleName();
        p.filter = r.getFilter();
        p.valueCutoff = r.valueCutoff;
    }

    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        if (null == p.valueCutoff) {
            p.writer.setValueLengthCutoff(DEFAULT_VALUE_LENGTH_CUTOFF);
        } else {
            p.writer.setValueLengthCutoff(p.valueCutoff);
        }

        try {
            addSearchResults(p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        p.map.put("title", p.query);
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return false;
    }

    protected void addSearchResults(final RequestParams p) throws IOException {
        Note n = p.queries.search(p.queryType, p.query, p.height, p.filter, p.style);
        addView(n, p);
    }

    protected class SearchRequest extends BasicSearchRequest {
        public final int valueCutoff;

        public SearchRequest(JSONObject json, Principal user) throws JSONException {
            super(json, user);

            valueCutoff = this.json.getInt(Params.VALUE_CUTOFF);
        }
    }
}
