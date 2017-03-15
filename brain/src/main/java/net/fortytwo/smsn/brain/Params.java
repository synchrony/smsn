package net.fortytwo.smsn.brain;

public interface Params {
    enum Format{json, wiki}

    String
            DEFAULT_SHARABILITY = "defaultSharability",
            DEFAULT_WEIGHT = "defaultWeight",
            FILTER = "filter",
            HEIGHT = "height",
            ID = "id",
            INCLUDE_TYPES = "includeTypes",
            MAX_RESULTS = "maxResults",
            MAX_SHARABILITY = "maxSharability",
            MAX_WEIGHT = "maxWeight",
            MIN_SHARABILITY = "minSharability",
            MIN_WEIGHT = "minWeight",
            NAME = "name",
            QUERY = "query",
            QUERY_TYPE = "queryType",
            REQUEST = "request",
            ROOT = "root",
            STYLE = "style",
            TITLE = "title",
            TITLE_CUTOFF = "titleCutoff",
            VIEW = "view",
            VIEW_FORMAT = "viewFormat",
            VIEW_TITLE = "title";
}
