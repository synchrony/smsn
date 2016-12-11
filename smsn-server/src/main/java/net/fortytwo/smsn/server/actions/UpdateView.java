package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.wiki.NoteReader;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.json.JSONException;
import org.json.JSONObject;

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
    public void parseRequest(final RequestParams p) throws IOException {
        p.setHeight(getHeight());
        // note: may be null
        p.setRootId(getRoot());
        p.setStyleName(getStyle());

        switch (getViewFormat()) {
            case json:
                try {
                    p.setJsonView(new JSONObject(getView()));
                } catch (JSONException e) {
                    throw new IOException(e);
                }
                break;
            case wiki:
                p.setWikiView(getView());
                break;
        }
        p.setFilter(getFilter());
    }

    @Override
    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        Note rootNote;

        if (null != p.getWikiView()) {
            try {
                try (InputStream in = new ByteArrayInputStream(p.getWikiView().getBytes())) {
                    rootNote = p.getParser().fromWikiText(in);
                }
            } catch (IOException | NoteReader.NoteParsingException e) {
                throw new RequestProcessingException(e);
            }
        } else if (null != p.getJsonView()) {
            try {
                rootNote = p.getParser().fromJSON(p.getJsonView());
            } catch (JSONException e) {
                throw new RequestProcessingException(e);
            }
        } else {
            throw new IllegalStateException();
        }

        rootNote.setId(p.getRootId());

        // Apply the update
        p.getQueries().update(rootNote, p.getHeight(), p.getFilter(), p.getStyle());

        // TODO: produce an appropriate view (e.g. a search) if the root is null
        Note n = null == p.getRoot()
                ? new Note()
                : p.getQueries().view(p.getRoot(), p.getHeight(), p.getFilter(), p.getStyle());
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
        return true;
    }
}
