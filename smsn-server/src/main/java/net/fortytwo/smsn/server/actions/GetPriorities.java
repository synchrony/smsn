package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for deriving a prioritized list of items in the knowledge base
 */
public class GetPriorities extends FilteredAction {

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

    @Override
    public void parseRequest(final RequestParams p) throws IOException {
        p.setFilter(getFilter());
        p.setMaxResults(getMaxResults());
    }

    @Override
    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {

        Note n = p.getQueries().priorityView(p.getFilter(), p.getMaxResults(), p.getBrain().getPriorities());
        try {
            addView(n, p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    @Override
    protected boolean doesRead() {
        return true;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }

}
