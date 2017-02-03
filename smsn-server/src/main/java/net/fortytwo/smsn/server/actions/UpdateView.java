package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;

import javax.validation.constraints.NotNull;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * A service for updating an Extend-o-Brain graph
 */
public class UpdateView extends RootedViewAction {

    @NotNull
    private String view;
    @NotNull
    private Params.Format viewFormat;

    public String getView() {
        return view;
    }

    public void setView(String view) {
        this.view = view;
    }

    public Params.Format getViewFormat() {
        return viewFormat;
    }

    public void setViewFormat(Params.Format viewFormat) {
        this.viewFormat = viewFormat;
    }

    @Override
    public void parseRequest(final RequestParams params) throws IOException {
        params.setHeight(getHeight());
        // note: may be null
        params.setRootId(getRoot());
        params.setStyleName(getStyle());

        params.setView(getView());

        params.setFilter(getFilter());
    }

    @Override
    protected void performTransaction(final RequestParams params) throws RequestProcessingException, BadRequestException {
        Note rootNote;

        if (null != params.getView()) {
            try {
                try (InputStream in = new ByteArrayInputStream(params.getView().getBytes())) {
                    rootNote = params.getWikiParser().parse(in);
                }
            } catch (IOException e) {
                throw new RequestProcessingException(e);
            }
        } else if (null != params.getView()) {
            try {
                rootNote = params.getJsonParser().parse(params.getView());
            } catch (IOException e) {
                throw new RequestProcessingException(e);
            }
        } else {
            throw new IllegalStateException();
        }

        rootNote.setId(params.getRootId());

        // Apply the update
        params.getQueries().update(rootNote, params.getHeight(), params.getFilter(), params.getStyle());

        // TODO: produce an appropriate view (e.g. a search) if the root is null
        Note n = null == params.getRoot()
                ? new Note()
                : params.getQueries().view(params.getRoot(), params.getHeight(), params.getFilter(), params.getStyle());
        try {
            addView(n, params);
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
        return true;
    }
}
