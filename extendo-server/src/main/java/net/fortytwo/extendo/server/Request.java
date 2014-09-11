package net.fortytwo.extendo.server;

import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Request {
    protected static final String
            DATASET = "dataset",
            DEFAULT_SHARABILITY = "defaultSharability",
            DEFAULT_WEIGHT = "defaultWeight",
            DEPTH = "depth",
            FILE = "file",
            FILTER = "filter",
            FORMAT = "format",
            ID = "id",
            INCLUDE_TYPES = "includeTypes",
            MAX_RESULTS = "maxResults",
            MAX_SHARABILITY = "maxSharability",
            MAX_WEIGHT = "maxWeight",
            MIN_SHARABILITY = "minSharability",
            MIN_WEIGHT = "minWeight",
            NAME = "name",
            QUERY = "query",
            ROOT = "root",
            STYLE = "style",
            VALUE = "value",
            VALUE_CUTOFF = "valueCutoff",
            VIEW = "view";

    protected final JSONObject json;
    protected final Principal user;

    public Request(final JSONObject json,
                   final Principal user) throws JSONException {
        this.json = json;
        this.user = user;
    }
}
