package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for finding root nodes of an Extend-o-Brain graph
 */
public class FindRoots extends BasicViewAction {

    @Override
    public void parseRequest(final RequestParams params) throws IOException {
        params.setHeight(getHeight());
        params.setStyleName(getStyle());
        params.setFilter(getFilter());
    }

    @Override
    protected void performTransaction(final RequestParams params) throws RequestProcessingException, BadRequestException {
        Note n = params.getQueries().findRootAtoms(params.getFilter(), params.getStyle(), params.getHeight() - 1);
        try {
            addView(n, params);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        params.getMap().put("title", "all roots");
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
