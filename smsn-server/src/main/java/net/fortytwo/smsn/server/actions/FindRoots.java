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
    public void parseRequest(final RequestParams p) throws IOException {
        p.setHeight(getHeight());
        p.setStyleName(getStyle());
        p.setFilter(getFilter());
    }

    @Override
    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        Note n = p.getQueries().findRootAtoms(p.getFilter(), p.getStyle(), p.getHeight() - 1);
        try {
            addView(n, p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        p.getMap().put("title", "all roots");
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
