package net.fortytwo.extendo.server;

import net.fortytwo.extendo.brain.Filter;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class FilteredResultsRequest extends Request {
    public final Filter filter;

    public FilteredResultsRequest(final JSONObject json,
                                  final Principal user) throws JSONException {
        super(json, user);

        filter = getFilter();
    }

    protected Filter getFilter() throws JSONException {
        JSONObject f = json.getJSONObject(FILTER);

        float defaultWeight = (float) f.optDouble(DEFAULT_WEIGHT, -1);
        float defaultSharability = (float) f.optDouble(DEFAULT_SHARABILITY, -1);
        float minWeight = (float) f.getDouble(MIN_WEIGHT);
        float maxWeight = (float) f.optDouble(MAX_WEIGHT, 1.0);

        float ms = (float) f.getDouble(MIN_SHARABILITY);
        float minSharability = ExtendoExtension.findMinAuthorizedSharability(user, ms);

        float maxSharability = (float) f.optDouble(MAX_SHARABILITY, 1.0);

        return new Filter(minWeight, maxWeight, defaultWeight, minSharability, maxSharability, defaultSharability);
    }
}
