package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;

/**
 * A service for receiving and internalizing events
 */
public class PushEvent extends Action {

    private String view;

    public String getView() {
        return view;
    }

    public void setView(String view) {
        this.view = view;
    }

    @Override
    public void parseRequest(final RequestParams params) throws IOException {
        params.setView(getView());
    }

    @Override
    protected void performTransaction(final RequestParams params) throws RequestProcessingException, BadRequestException {
        Note event;
        try {
            event = params.getJsonParser().parse(params.getView());
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }

        params.getBrain().getEventStack().push(event);
    }

    @Override
    protected boolean doesRead() {
        return false;
    }

    @Override
    protected boolean doesWrite() {
        // pushing of events is currently not considered writing... to the graph
        return false;
    }

}
