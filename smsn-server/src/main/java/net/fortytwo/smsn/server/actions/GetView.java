package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.actions.requests.ViewRequest;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for retrieving hierarchical views of Extend-o-Brain graphs
 */
public class GetView extends Action<ViewRequest> {

    @Override
    public String getName() {
        return "view";
    }

    @Override
    public void parseRequest(final ViewRequest request, final RequestParams params) throws IOException {
        params.setHeight(request.getHeight());
        params.setRootId(request.getRoot());
        params.setStyleName(request.getStyle());
        params.setFilter(request.getFilter());
        params.setIncludeTypes(request.isIncludeTypes());
    }

    @Override
    protected void performTransaction(final RequestParams params)
            throws RequestProcessingException, BadRequestException {

        Note note = params.getQueries().view(params.getRoot(), params.getHeight(), params.getFilter(), params.getStyle());
        try {
            addView(note, params);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        addToHistory(params.getRootId());
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
