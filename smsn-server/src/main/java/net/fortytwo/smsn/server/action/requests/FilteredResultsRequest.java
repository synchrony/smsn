package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.Request;
import org.json.JSONException;
import org.json.JSONObject;

public class FilteredResultsRequest extends Request {
    private final Filter filter;

    public FilteredResultsRequest(final JSONObject json) throws JSONException {
        super(json);

        filter = constructFilter();
    }

    public Filter getFilter() {
        return filter;
    }

    private Filter constructFilter() throws JSONException {
        JSONObject f = json.getJSONObject(Params.FILTER);

        float defaultWeight = (float) f.optDouble(Params.DEFAULT_WEIGHT, -1);
        float defaultSharability = (float) f.optDouble(Params.DEFAULT_SHARABILITY, -1);
        float minWeight = (float) f.getDouble(Params.MIN_WEIGHT);
        float maxWeight = (float) f.optDouble(Params.MAX_WEIGHT, 1.0);

        float minSharability = Action.findMinAuthorizedSharability((float) f.getDouble(Params.MIN_SHARABILITY));
        float maxSharability = (float) f.optDouble(Params.MAX_SHARABILITY, 1.0);

        return new Filter(minWeight, maxWeight, defaultWeight, minSharability, maxSharability, defaultSharability);
    }
}
