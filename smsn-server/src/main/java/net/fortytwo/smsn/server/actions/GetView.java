package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for retrieving hierarchical views of Extend-o-Brain graphs
 */
public class GetView extends RootedViewAction {

    private boolean includeTypes = false;

    public boolean isIncludeTypes() {
        return includeTypes;
    }

    public void setIncludeTypes(boolean includeTypes) {
        this.includeTypes = includeTypes;
    }

    @Override
    public void parseRequest(final RequestParams params) throws IOException {
        params.setHeight(getHeight());
        params.setRootId(getRoot());
        params.setStyleName(getStyle());
        params.setFilter(getFilter());
        params.setIncludeTypes(isIncludeTypes());
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
