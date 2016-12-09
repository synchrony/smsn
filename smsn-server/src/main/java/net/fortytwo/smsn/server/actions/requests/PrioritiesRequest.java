package net.fortytwo.smsn.server.actions.requests;

import net.fortytwo.smsn.brain.Params;

public class PrioritiesRequest extends FilteredResultsRequest {

    private int maxResults = 100;

    public int getMaxResults() {
        return maxResults;
    }

    public void setMaxResults(int maxResults) {
        if (maxResults <= 0) {
            throw new IllegalArgumentException(Params.MAX_RESULTS + " parameter must be a positive integer");
        }

        this.maxResults = maxResults;
    }
}
