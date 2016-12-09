package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.actions.requests.PrioritiesRequest;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for deriving a prioritized list of items in the knowledge base
 */
public class GetPriorities extends Action<PrioritiesRequest> {

    @Override
    public String getName() {
        return "priorities";
    }

    @Override
    public void parseRequest(final PrioritiesRequest request, final RequestParams p) throws IOException {
        p.setFilter(request.getFilter());
        p.setMaxResults(request.getMaxResults());
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
