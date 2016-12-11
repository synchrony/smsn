package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import java.io.IOException;
import java.util.List;

/**
 * A service for retrieving the stack of recently pushed events
 */
public class GetEvents extends Action {

    private int height;

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }

    @Override
    public void parseRequest(final RequestParams p) throws IOException {
        p.setHeight(getHeight());
    }

    @Override
    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        List<Note> events = p.getBrain().getEventStack().getEvents();

        Note view = new Note();
        view.setValue("event stack");

        for (Note n : events) {
            Note e = new Note(n);
            e.truncate(p.getHeight());
            view.addChild(e);
        }

        try {
            addView(view, p);
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        }
    }

    @Override
    protected boolean doesRead() {
        // getting events is currently not considered reading... from the graph
        return false;
    }

    @Override
    protected boolean doesWrite() {
        return false;
    }
}
