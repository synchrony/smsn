package net.fortytwo.smsn.server.actions.requests;

import net.fortytwo.smsn.brain.NoteQueries;

import javax.validation.constraints.NotNull;

public class BasicSearchRequest extends BasicViewRequest {

    @NotNull
    private String query;
    @NotNull
    private NoteQueries.QueryType queryType;

    public NoteQueries.QueryType getQueryType() {
        return queryType;
    }

    public String getQuery() {
        return query;
    }

    public void setQuery(String query) {
        // TODO: this doesn't solve the problem (that you can't search on queries with extended characters)
        //query = new String(query.getBytes(), "UTF-8");

        this.query = query;
    }

    public void setQueryType(NoteQueries.QueryType queryType) {
        this.queryType = queryType;
    }
}
